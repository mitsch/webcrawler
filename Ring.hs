module Ring (
	RingChan,
	OutputChan,
	RingNode,
	forward,
	forwardList,
	runRing,
	runRingS,
	writeRingChan,
	writeListRingChan,
	signalLastRingChan,
	signalClosingRingChan,
	runOutput,
	writeOutputChan,
	writeListOutputChan,
	closeOutputChan
) where

import Prelude hiding (log)
import Control.Concurrent.Chan
import System.IO()
import Control.Monad
import Control.Monad.Trans.Class

-- signal carrier for ring system
data RingCarrier a = RingValue a | RingLastValue | RingClosing

-- ring channel
type RingChan a = Chan (RingCarrier a)

writeRingChan :: RingChan a -> a -> IO ()
writeRingChan c x = writeChan c $ RingValue x

writeListRingChan :: RingChan a -> [a] -> IO ()
writeListRingChan c x = writeList2Chan c $ map RingValue x

signalLastRingChan :: RingChan a -> IO ()
signalLastRingChan c = writeChan c RingLastValue

signalClosingRingChan :: RingChan a -> IO ()
signalClosingRingChan c = writeChan c RingClosing

-- signal carrier for output system
data OutputCarrier a = OutputValue a | OutputClosedSource

-- output channel
type OutputChan a = Chan (OutputCarrier a)

-- writes value into output channel
writeOutputChan :: OutputChan a -> a -> IO ()
writeOutputChan c x = writeChan c $ OutputValue x

-- writes values into output channel
writeListOutputChan :: OutputChan a -> [a] -> IO ()
writeListOutputChan c xs = writeList2Chan c $ map OutputValue xs

-- closes source of output
closeOutputChan :: OutputChan a -> IO ()
closeOutputChan c = writeChan c OutputClosedSource

-- state of ring node process
newtype RingNode a m b = RingNode { runRingNode :: m ([a], b)}

-- implementation of Monad for RingNode
instance (Monad m) => Monad (RingNode a m) where
	return x = RingNode $ return ([], x)
	r >>= g   = RingNode $ do
	           	(a, b) <- runRingNode r
	           	(c, d) <- runRingNode $ g b
	           	return (a ++ c, d)
	fail s = RingNode (fail s)

-- implementat of MonadTrans for RingNode
instance MonadTrans (RingNode a) where
	lift = RingNode . liftM (\x -> ([], x))

-- forward some token to the next process in the ring
forward :: Monad m => a -> RingNode a m ()
forward x = RingNode $ return ([x], ())

-- forward list  of some token to the next process in the ring
forwardList :: Monad m => [a] -> RingNode a m ()
forwardList x = RingNode $ return (x, ())

-- strict looping over some state a with option to the end the loop
loop_ :: Monad m => a -> (a -> m (a, Bool)) -> m a
loop_ i f = g i
	where g x = f x >>= (\(v, b) -> if b then v `seq` g v else return v)

-- carries out a ring node with some state for the handler routine f
runRingS :: RingChan a -> RingChan b -> e -> (e -> a -> RingNode b IO e) -> IO e
runRingS x y i f = fmap snd $ loop_ (False, i) $ \(gotLastEntry, state) -> do
	nextCarrier <- readChan x
	case nextCarrier of
		RingValue v -> do
			(ys, newState) <- runRingNode (f state v)
			writeListRingChan y ys
			return ((False, newState), True)
		RingLastValue -> do
			if gotLastEntry
			then signalClosingRingChan y >> return ((True, state), False)
			else signalLastRingChan y >> return ((True, state), True)
		RingClosing -> signalClosingRingChan y >> return ((True, state), False)

-- carries out a ring node with stateless hanlder routine f
runRing :: RingChan a -> RingChan b -> (a -> RingNode b IO ()) -> IO ()
runRing x y f = void $ runRingS x y () (\_ a -> f a)

-- carries out output sink node
runOutput :: OutputChan a -> Int -> (a -> IO ()) -> IO ()
runOutput c m f = g m
	where g n = do
		if n == 0 then return ()
		else readChan c >>= \t -> case t of
			OutputValue v -> f v >>= \y -> y `seq` (g n)
			OutputClosedSource -> g $ n - 1
