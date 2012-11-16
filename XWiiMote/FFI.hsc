{-# LANGUAGE ForeignFunctionInterface #-}

module XWiiMote.FFI where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import Data.Word
import Data.Int

import Control.Monad

#include <sys/time.h>
#include <xwiimote.h>

-- Let's get a definition for Timeval out of the way first.
-- Definition borrowed from:
-- http://hackage.haskell.org/packages/archive/hsdns/1.1/doc/html/System-Posix-GetTimeOfDay.html
data Timeval = Timeval CTime #{type suseconds_t}
    deriving (Show)

instance Storable Timeval where
  sizeOf _    = #size struct timeval
  alignment _ = #const __alignof__(struct timeval)
  poke ptr (Timeval t us) = do
    (#poke struct timeval, tv_sec) ptr t
    (#poke struct timeval, tv_usec) ptr us
  peek ptr = do
    t <- (#peek struct timeval, tv_sec) ptr
    us <- (#peek struct timeval, tv_usec) ptr
    return (Timeval t us)

data XWiiEventTypes = Key | Accel | Ir
  deriving (Show, Eq)

instance Enum XWiiEventTypes where
  fromEnum Key = #const XWII_EVENT_KEY
  fromEnum Accel = #const XWII_EVENT_ACCEL
  fromEnum Ir = #const XWII_EVENT_IR
  toEnum (#const XWII_EVENT_KEY) = Key
  toEnum (#const XWII_EVENT_ACCEL) = Accel
  toEnum (#const XWII_EVENT_IR) = Ir
  toEnum _ = error "Invalid XWiiEventType"

data XWiiEventKeys = KeyLeft
                   | KeyRight
                   | KeyUp
                   | KeyDown
                   | KeyA
                   | KeyB
                   | KeyPlus
                   | KeyMinus
                   | KeyHome
                   | KeyOne
                   | KeyTwo  
  deriving (Show, Eq)

instance Enum XWiiEventKeys where
  fromEnum KeyLeft  = #const XWII_KEY_LEFT
  fromEnum KeyRight = #const XWII_KEY_RIGHT
  fromEnum KeyUp    = #const XWII_KEY_UP
  fromEnum KeyDown  = #const XWII_KEY_DOWN
  fromEnum KeyA     = #const XWII_KEY_A
  fromEnum KeyB     = #const XWII_KEY_B
  fromEnum KeyPlus  = #const XWII_KEY_PLUS
  fromEnum KeyMinus = #const XWII_KEY_MINUS
  fromEnum KeyHome  = #const XWII_KEY_HOME
  fromEnum KeyOne   = #const XWII_KEY_ONE
  fromEnum KeyTwo   = #const XWII_KEY_TWO
  toEnum (#const XWII_KEY_LEFT)  = KeyLeft
  toEnum (#const XWII_KEY_RIGHT) = KeyRight
  toEnum (#const XWII_KEY_UP)    = KeyUp
  toEnum (#const XWII_KEY_DOWN)  = KeyDown
  toEnum (#const XWII_KEY_A)     = KeyA
  toEnum (#const XWII_KEY_B)     = KeyB
  toEnum (#const XWII_KEY_PLUS)  = KeyPlus
  toEnum (#const XWII_KEY_MINUS) = KeyMinus
  toEnum (#const XWII_KEY_HOME)  = KeyHome
  toEnum (#const XWII_KEY_ONE)   = KeyOne
  toEnum (#const XWII_KEY_TWO)   = KeyTwo
  toEnum _ = error "Invalid XWiiEventKeys"

data XWiiEventKey = XWiiEventKey {
    code  :: (#type unsigned int),
    state :: (#type unsigned int)
} deriving (Show)

instance Storable XWiiEventKey where
    sizeOf _ = #const sizeof(struct xwii_event_key)
    alignment _ = #const __alignof__(struct xwii_event_key)
    peek ptr = liftM2 XWiiEventKey ((#peek struct xwii_event_key, code) ptr)
                                   ((#peek struct xwii_event_key, state) ptr)
    poke ptr e = do
        (#poke struct xwii_event_key, code) ptr (code e)
        (#poke struct xwii_event_key, state) ptr (state e)

data XWiiEventAbs = XWiiEventAbs {
    absX :: (#type int32_t),
    absY :: (#type int32_t),
    absZ :: (#type int32_t)
} deriving (Show)

instance Storable XWiiEventAbs where
    sizeOf _ = #const sizeof(struct xwii_event_abs)
    alignment _ = #const __alignof__(struct xwii_event_abs)
    peek ptr = liftM3 XWiiEventAbs ((#peek struct xwii_event_abs, x) ptr)
                                   ((#peek struct xwii_event_abs, y) ptr)
                                   ((#peek struct xwii_event_abs, z) ptr)
    poke ptr e = do
        (#poke struct xwii_event_abs, x) ptr (absX e)
        (#poke struct xwii_event_abs, y) ptr (absY e)
        (#poke struct xwii_event_abs, z) ptr (absZ e)

type XWiiEventAbsQuad = (XWiiEventAbs, XWiiEventAbs, XWiiEventAbs, XWiiEventAbs)

data WiiEvents = KeyEvent   XWiiEventKey
               | AccelEvent XWiiEventAbsQuad
               | IrEvent    XWiiEventAbsQuad
    deriving (Show)

wiiEventToCType :: WiiEvents -> (#type unsigned int)
wiiEventToCType (KeyEvent _) = toEnum . fromEnum $ Key
wiiEventToCType (AccelEvent _) = toEnum . fromEnum $ Accel
wiiEventToCType (IrEvent _) = toEnum . fromEnum $ Ir

data XWiiEvent = XWiiEvent {
    time :: Timeval,
    event :: WiiEvents
} deriving (Show)

instance Storable XWiiEvent where
    sizeOf _ = #const sizeof(struct xwii_event)
    alignment _ = #const __alignof__(struct xwii_event)
    peek ptr = do
        time <- (#peek struct xwii_event, time) ptr
        t <- (#peek struct xwii_event, type) ptr :: IO (#type unsigned int)
        ev <- case (toEnum . fromEnum $ t) of
                Key -> liftM KeyEvent $ (#peek struct xwii_event, v.key) ptr
                Accel -> do
                    a0 <- (#peek struct xwii_event, v.abs[0]) ptr
                    a1 <- (#peek struct xwii_event, v.abs[1]) ptr
                    a2 <- (#peek struct xwii_event, v.abs[2]) ptr
                    a3 <- (#peek struct xwii_event, v.abs[3]) ptr
                    return $ AccelEvent (a0, a1, a2, a3)
                Ir -> do
                    a0 <- (#peek struct xwii_event, v.abs[0]) ptr
                    a1 <- (#peek struct xwii_event, v.abs[1]) ptr
                    a2 <- (#peek struct xwii_event, v.abs[2]) ptr
                    a3 <- (#peek struct xwii_event, v.abs[3]) ptr
                    return $ IrEvent (a0, a1, a2, a3)
        return $ XWiiEvent time ev
    poke ptr e = do
        (#poke struct xwii_event, time) ptr (time e)
        (#poke struct xwii_event, time) ptr (wiiEventToCType $ event e)
        case event e of
            KeyEvent k -> do
                (#poke struct xwii_event, v.key) ptr k
            AccelEvent (a0, a1, a2, a3) -> do
                (#poke struct xwii_event, v.abs[0]) ptr a0
                (#poke struct xwii_event, v.abs[1]) ptr a1
                (#poke struct xwii_event, v.abs[2]) ptr a2
                (#poke struct xwii_event, v.abs[3]) ptr a3
            IrEvent (a0, a1, a2, a3) -> do
                (#poke struct xwii_event, v.abs[0]) ptr a0
                (#poke struct xwii_event, v.abs[1]) ptr a1
                (#poke struct xwii_event, v.abs[2]) ptr a2
                (#poke struct xwii_event, v.abs[3]) ptr a3

data XWiiIface = XWiiIface IntPtr
    deriving (Show)

instance Storable XWiiIface where
    sizeOf _ = #const sizeof(struct xwii_iface *)
    alignment _ = #const __alignof__(struct wii_iface *)
    peek ptr = liftM XWiiIface (peekByteOff ptr 0)
    poke ptr (XWiiIface i) = pokeByteOff ptr 0 i

foreign import ccall "xwiimote.h xwii_iface_new"
    ffi_xwii_iface_new :: Ptr XWiiIface
                       -> Ptr #type const char
                       -> IO #type int

foreign import ccall "xwiimote.h xwii_iface_ref"
    ffi_xwii_iface_ref :: Ptr ()
                       -> IO ()

foreign import ccall "xwiimote.h xwii_iface_unref"
    ffi_xwii_iface_unref :: Ptr ()
                         -> IO ()

foreign import ccall "xwiimote.h xwii_iface_get_fd"
    ffi_xwii_iface_get_fd :: Ptr ()
                          -> IO #type int

foreign import ccall "xwiimote.h xwii_iface_open"
    ffi_xwii_iface_open :: Ptr ()
                        -> #type unsigned int
                        -> IO #type int

foreign import ccall "xwiimote.h xwii_iface_close"
    ffi_xwii_iface_close :: Ptr ()
                         -> #type unsigned int
                         -> IO ()

foreign import ccall "xwiimote.h xwii_iface_opened"
    ffi_xwii_iface_opened :: Ptr ()
                          -> IO #type unsigned int

foreign import ccall "xwiimote.h xwii_iface_poll"
    ffi_xwii_iface_poll :: Ptr ()
                        -> Ptr XWiiEvent
                        -> IO #type int

foreign import ccall "xwiimote.h xwii_iface_rumble"
    ffi_xwii_iface_rumble :: Ptr ()
                          -> #type bool
                          -> IO #type int

data XWiiMonitor = XWiiMonitor IntPtr
    deriving (Show)

instance Storable XWiiMonitor where
    sizeOf _ = #const sizeof(struct xwii_monitor *)
    alignment _ = #const __alignof__(struct wii_monitor *)
    peek ptr = liftM XWiiMonitor (peekByteOff ptr 0)
    poke ptr (XWiiMonitor i) = pokeByteOff ptr 0 i

foreign import ccall "xwiimote.h xwii_monitor_new"
    ffi_xwii_monitor_new :: #type bool
                         -> #type bool
                         -> IO (Ptr XWiiMonitor)

foreign import ccall "xwiimote.h xwii_monitor_ref"
    ffi_xwii_monitor_ref :: Ptr ()
                         -> IO ()

foreign import ccall "xwiimote.h xwii_monitor_unref"
    ffi_xwii_monitor_unref :: Ptr ()
                           -> IO ()

foreign import ccall "xwiimote.h xwii_monitor_get_fd"
    ffi_xwii_monitor_get_fd :: Ptr ()
                            -> #type bool
                            -> IO #type int

foreign import ccall "xwiimote.h xwii_monitor_poll"
    ffi_xwii_monitor_poll :: Ptr ()
                          -> IO (Ptr #type char)

xwiiIfaceNew :: String -> IO (Int32, XWiiIface)
xwiiIfaceNew syspath = withCString (\c_str -> alloca (\ptr -> withIfacePtr ptr c_str))
  where withPtrs ptr c_str = do
          ret <- ffi_xwii_iface_new ptr c_str
          iface <- peek ptr
          return (ret, iface)
