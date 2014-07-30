module ChanClass (ChanClass, writeChan, writeList2Chan, readChan) where


import qualified Control.Concurrent.Chan as C
import qualified Control.Concurrent.BoundedChan as B

class ChanClass c where
	writeChan :: c a -> a -> IO ()
	writeList2Chan :: c a -> [a] -> IO ()
	readChan :: c a -> IO a

instance ChanClass (C.Chan) where
	writeChan = C.writeChan
	writeList2Chan = C.writeList2Chan
	readChan = C.readChan

instance ChanClass (B.BoundedChan) where
	writeChan = B.writeChan
	writeList2Chan = B.writeList2Chan
	readChan = B.readChan
