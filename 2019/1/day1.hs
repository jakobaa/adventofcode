type Mass = Int
type Fuel = Int

calcFuel :: Mass -> Fuel
calcFuel m = (m `div` 3) - 2

main :: IO ()
main = readFile "input.txt" >>=
          print . foldr ((+) . fuelForFuel . calcFuel) 0 . map read . lines

fuelForFuel :: Fuel -> Fuel
fuelForFuel fuel
  | fuel > 0  = fuel + fuelForFuel (calcFuel fuel)
  | otherwise = 0
