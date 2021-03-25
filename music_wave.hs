import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable
import System.Process
import Text.Printf
import Data.List

type Seconds = Float
type Samples = Float
type Hz = Float
type Pulse = Float
type Semitones = Float
type Beats = Float

outputFilePath :: FilePath 
outputFilePath = "output.bin"

volume :: Float 
volume = 0.2

sampleRate :: Samples 
sampleRate = 48000

pitchStandard :: Hz
pitchStandard = 440

bpm :: Beats
bpm = 120

beatDuration :: Seconds
beatDuration = 60 / bpm

f :: Semitones -> Hz
f n = pitchStandard * (2 ** (1 / 12)) ** n

note :: Semitones -> Beats -> [Pulse]
note n beats = freq (f n) (beats * beatDuration)

freq :: Hz -> Seconds -> [Pulse]
freq hz duration = map (* volume) $ zipWith3 (\x y z -> x * y * z) release attack output
    where
        step = (hz * 2 * pi) / sampleRate
        attack :: [Pulse]
        attack = map (min 1) [0, 0.001..]
        release :: [Pulse]
        release = reverse $ take (length output) attack
        output :: [Pulse]
        output = map (sin . (* step)) [0.0..sampleRate * duration]

wave :: [Pulse]
wave = concat [
    note 0 0.25,
    note 0 0.25,
    note 0 0.25,
    note 0 0.25,
    note 0 0.5,

    note 0 0.25,
    note 0 0.25,
    note 0 0.25,
    note 0 0.25,
    note 0 0.25,
    note 0 0.25,
    note 0 0.5,

    note 5 0.25,
    note 5 0.25,
    note 5 0.25,
    note 5 0.25,
    note 5 0.25,
    note 5 0.25,
    note 5 0.5,

    note 3 0.25,
    note 3 0.25,
    note 3 0.25,
    note 3 0.25,
    note 3 0.25,
    note 3 0.25,
    note 3 0.5,
    
    note (-2) 0.5,
    note 0 0.25,
    note 0 0.25,
    note 0 0.25,
    note 0 0.25,
    note 0 0.5 ]

save :: FilePath -> IO ()
save filepath = B.writeFile filepath $ B.toLazyByteString $ foldMap B.floatLE wave

play :: IO ()
play = do
    save outputFilePath
    _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
    return ()
