-- @Author: ariesduanmu
-- @Date:   2019-03-04 23:57:27
-- @Last Modified by:   ariesduanmu
-- @Last Modified time: 2019-03-05 00:59:24
import Test.QuickCheck

type Picture = [[Char]]
flipH :: Picture -> Picture
flipH = reverse

above :: Picture -> Picture -> Picture
above = (++)

flipV :: Picture -> Picture
flipV pic = [reverse line | line <- pic]

beside :: Picture -> Picture -> Picture
beside picL picR = [lineL++lineR | (lineL, lineR) <- zip picL picR]

invertChar :: Char -> Char
invertChar ch = if ch=='.' then '#' else '.'

invertLine :: [Char] -> [Char]
invertLine line = [invertChar ch|ch <- line]

invertColour :: Picture -> Picture
invertColour pic = [invertLine line | line <- pic]

prop_AboveFlipV :: Picture -> Picture -> Bool
prop_AboveFlipV pic1 pic2 = flipV (pic1 `above` pic2) == (flipV pic1) `above` (flipV pic2)

-- TODO: NEED FIX
prop_AboveFlipH :: Picture -> Picture -> Bool
prop_AboveFlipH pic1 pic2 = flipH (pic1 `above` pic2) == (flipH pic1) `above` (flipH pic2)

