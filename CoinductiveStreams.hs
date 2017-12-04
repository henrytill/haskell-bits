{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}
-- |
-- Copattern/coinductive Programming in Haskell
--
-- <https://www.reddit.com/r/haskell/comments/4aju8f/simple_example_of_emulating_copattern_matching_in/>
--
module CoinductiveStreams where

-- |
-- StreamModel is what we ordinarily write as "Stream" in haskell, though is not
-- ideal for a variety of reasons.
--
-- StreamModel is not fundamental to this approach.  It it only included for
-- simplicity's sake.
data StreamModel a = StreamModel { head :: a , tail :: StreamModel a}

data StreamTag a {- param list -} res {- result type!  -} where
  HeadTag :: StreamTag a a
  TailTag :: StreamTag a (Stream a)

newtype Stream a = Stream { unStream  :: (forall res . StreamTag a res -> res) }

headStream :: Stream a -> a
headStream (Stream f) = f HeadTag

tailStream :: Stream a -> Stream a
tailStream (Stream f) = f TailTag

-- | zipWith for Stream
rawRawZipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
rawRawZipWith f sta stb = Stream $ \ x ->
  case x of
    HeadTag -> f (headStream sta) (headStream stb)
    TailTag -> rawRawZipWith f (tailStream sta) (tailStream stb)

-- | Convert from Stream (the coinductive definition) to StreamModel (the usual
-- stream definition in haskell)
stream2StreamModel :: Stream a -> StreamModel a
stream2StreamModel s = StreamModel (headStream s) (stream2StreamModel $ tailStream s)
