{-# LANGUAGE DataKinds #-}

-- rhine
import FRP.Rhine

-- base
--import Control.Monad.IO.Class (liftIO)
import Data.Void


type Second = Millisecond 1000
type Second2 = Millisecond 2000

throwOnInput :: ClSF (ExceptT String IO) (IOClock (ExceptT e IO) StdinClock) () () 
throwOnInput = tagS >>> throwS

title :: Rhine (ExceptT e IO) (IOClock (ExceptT e IO) Second) () ()
title = constMCl (liftIO $ putStrLn "Title") @@ HoistClock waitClock liftIO

instruction :: Rhine (ExceptT e IO) (IOClock (ExceptT e IO) Second2) () ()
instruction = constMCl (liftIO $ putStrLn "Press enter to start") @@ HoistClock waitClock liftIO

menu :: Rhine (ExceptT e IO)
  (ParClock
    (ExceptT e IO)
    (IOClock (ExceptT e IO) Second)
    (IOClock (ExceptT e IO) Second2)
  ) () ()
menu = title ||@ concurrentlyExcept @|| instruction

mainMenu :: Rhine (ExceptT String IO)
  (ParClock
    (ExceptT String IO)
    (IOClock (ExceptT String IO) StdinClock)
    (ParClock
      (ExceptT String IO)
      (IOClock (ExceptT String IO) Second)
      (IOClock (ExceptT String IO) Second2)
    )
  )
  () ()
mainMenu = throwOnInput @@ HoistClock StdinClock liftIO ||@ concurrentlyExcept @|| menu

gameLoop :: Rhine IO Second () ()
gameLoop = constMCl (putStrLn "playing the game...") @@ waitClock

game :: RhineExcept IO () () Void
game = OnException (Try mainMenu) (Safe gameLoop)

main :: IO ()
main = flowExcept game
