-- 1 bottle of wine = 4 drinks
-- 1 bottle of liquor = 18 drinks
-- 1 bottle of beer = 1 drink
-- 1 keg of beer = 165 drinks

-- standard bar: 50% wine, 20% beer, 30% liquor
-- wine and beer only: 75% wine, 25% beer, 0% liquor
-- Expect 1 drink per hour per guest
-- If you're doing one signature cocktail,

data Bar = Bar {
    beer   :: Double
  , wine   :: Double
  , liquor :: Double
} deriving (Show)

wineAndBeer :: Bar
wineAndBeer = Bar 0.25 0.75 0

standardBar :: Bar
standardBar = Bar 0.2 0.5 0.3

beerCount :: Num a => a -> a -> a
beerCount percent drinks = percent * drinks

wineCount :: Fractional a => a -> a -> a
wineCount percent drinks = percent * drinks / 4

liquorCount :: Fractional a => a -> a -> a
liquorCount percent drinks = percent * drinks / 18

barBreakdown :: Bar -> Double -> Bar
barBreakdown bT drinks = Bar b w l
  where b = beerCount (beer bT) drinks
        w = wineCount (wine bT) drinks
        l = liquorCount (liquor bT) drinks

standardBarBreakdown :: Double -> Bar
standardBarBreakdown = barBreakdown standardBar

wineAndBeerBreakdown :: Double -> Bar
wineAndBeerBreakdown = barBreakdown wineAndBeer

calculateBar :: Double -> Double -> Double -> Double -> Bar
calculateBar ppl hrs sig bar = breakdown
  where drinks = if sig == 1
                  then ppl * (hrs - 1)
                  else ppl * hrs
        breakdown = if bar == 1
                      then standardBarBreakdown drinks
                      else wineAndBeerBreakdown drinks

printStatement :: Bar -> String
printStatement bar =
  show (beer bar) ++ " bottles of beer, " ++
  show (wine bar) ++ " bottles of wine, " ++
  "and " ++ show (liquor bar) ++ " bottles of liquor (pick your poison)."


main :: IO ()
main = do
  putStrLn "How many people are you expecting?"
  ppl <- read <$> getLine

  putStrLn "How many hours will your reception last?"
  hrs <- read <$> getLine

  putStrLn "Are you planning on doing a siguature cocktail?"
  putStrLn "[1] Yes"
  putStrLn "[2] No"
  signature <- read <$> getLine

  putStrLn "What type of bar will you have? (Enter the corresponding number)"
  putStrLn "[1] Modified Full Bar"
  putStrLn "[2] Wine & Beer Only"
  bar <- read <$> getLine

  let breakdown = calculateBar ppl hrs signature bar
  if signature == 1
    then print $ printStatement breakdown ++
                      " Make sure you have enough ingredients to make " ++
                      show ppl ++ " signature cocktails for happy hour."
    else print $ printStatement breakdown
