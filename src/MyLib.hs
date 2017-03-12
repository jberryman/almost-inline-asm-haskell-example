{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RecordWildCards, BangPatterns, CPP #-}
#ifdef USING_x86_64_ASM_SIPROUND
{-# LANGUAGE GHCForeignImportPrim, ForeignFunctionInterface, MagicHash, UnboxedTuples, UnliftedFFITypes #-}
#endif

module MyLib (sipRounds) where


-- We use the identity monad for non-recursive binding and utilize name
-- shadowing and RecordWildcards so we can easily translate the implicitly
-- stateful siphash reference implementation here:
--
-- https://github.com/veorq/SipHash/blob/master/siphash24.c
--  
import Data.Functor.Identity
import Data.Word (Word64)
import Data.Bits
import Control.Exception(assert)

#ifdef USING_x86_64_ASM_SIPROUND
import GHC.Prim
import GHC.Word
#endif


{-# INLINE sipRounds #-}
sipRounds :: Int -> Word64 -> Word64 -> Word64 -> Word64 -> Identity (Word64, Word64, Word64, Word64)

#ifdef USING_x86_64_ASM_SIPROUND

-------------  ASM SIPROUNDS THAT LET US EMIT ROTATE  -------------

-- see `sipround_x86_64.S`
#define sipRoundImport(sipRound_s_xNUM) \
foreign import prim "sipRound_s_xNUM"   \
  sipRound_s_xNUM# :: Word# -> Word# -> Word# -> Word# -> (# Word#, Word#, Word#, Word# #)   ;\
;\
sipRound_s_xNUM ::  Word64 -> Word64 -> Word64 -> Word64 -> (Word64, Word64, Word64, Word64)   ;\
{-# INLINE sipRound_s_xNUM #-}   ;\
sipRound_s_xNUM (W64# v0) (W64# v1) (W64# v2) (W64# v3) = case sipRound_s_xNUM# v0 v1 v2 v3 of   ;\
  (# v0', v1', v2', v3' #) -> (W64# v0', W64# v1', W64# v2', W64# v3')

-- NOTE: the overhead of the requisite MOVs and jumps caused this to be a regression 
-- sipRoundImport(sipRound_s_x1)
-- sipRoundImport(sipRound_s_x2)
sipRoundImport(sipRound_s_x3)
sipRoundImport(sipRound_s_x4)

sipRounds 0 = error "The number of rounds must be > 0" 
-- The constant overhead of a bunch of extraneous movs and function call etc,
-- seem to make our single asm sipRound not worthwhile at the moment:
sipRounds 1 = \v0 v1 v2 v3 -> do
    sipRound v0 v1 v2 v3
sipRounds 2 = \v0 v1 v2 v3 -> do 
    (v0,v1,v2,v3) <- sipRound v0 v1 v2 v3
    sipRound v0 v1 v2 v3
sipRounds 3 = \v0 v1 v2 v3 ->
    return $ sipRound_s_x3 v0 v1 v2 v3
sipRounds 4 = \v0 v1 v2 v3 ->
    return $ sipRound_s_x4 v0 v1 v2 v3

---------------------   END ASM STUFF     ---------------------------

#endif

sipRounds n = go n where
  go 0 v0 v1 v2 v3 = return (v0,v1,v2,v3)
  go n' v0 v1 v2 v3 = do
    (v0,v1,v2,v3) <- sipRound v0 v1 v2 v3
    go (n'-1) v0 v1 v2 v3




------------------   PURE HASKELL SIPROUNDS   ---------------------


-- NOTE: a bitwise rotate idiom. We expect a C compiler to turn this into an
-- intrinsic. The LLVM backend does this for us, but the native backend doesn't
-- do much "strength reduction"-style optimizations of this sort, and doesn't
-- support a rotate primop:
rotl :: Word64 -> Int -> Word64
{-# INLINE rotl #-}
rotl x b = assert (b > 0 && b < 64) $
    (x `unsafeShiftL` b) .|. (x `unsafeShiftR` (64 - b))

sipRound :: Word64 -> Word64 -> Word64 -> Word64 -> Identity (Word64, Word64, Word64, Word64)
{-# INLINE[2] sipRound #-}
sipRound v0 v1 v2 v3 = do
    v0 <- return $ v0 + v1 
    v1 <- return $ rotl v1 13
    v1 <- return $ v1 `xor` v0
    v0 <- return $ rotl v0 32

    v2 <- return $ v2 + v3
    v3 <- return $ rotl v3 16
    v3 <- return $ v3 `xor` v2

    v0 <- return $ v0 + v3
    v3 <- return $ rotl v3 21
    v3 <- return $ v3 `xor` v0

    v2 <- return $ v2 + v1
    v1 <- return $ rotl v1 17
    v1 <- return $ v1 `xor` v2
    v2 <- return $ rotl v2 32
    return (v0, v1, v2, v3)
