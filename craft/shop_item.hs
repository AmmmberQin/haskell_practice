-- @Author: ariesduanmu
-- @Date:   2019-03-03 23:50:58
-- @Last Modified by:   ariesduanmu
-- @Last Modified time: 2019-03-04 00:52:54
import Excerise5

type ItemName = String
type Price = Int
type BarCode = Int

type Database = [(BarCode, ItemName, Price)]

codeIndex :: Database
codeIndex = [(4719, "Fish Fingers", 121),
             (5643, "Nappies", 1010),
             (3814, "Orange Jelly", 56),
             (1111, "Hula Hoops", 21),
             (1112, "Hula Hoops(Giant", 133),
             (1234, "Dry Sherry, 1lt", 540)]

type TillType = [BarCode]
type BillType = [(ItemName, Price)]

makeBill :: TillType -> BillType
makeBill tilltype = [(n,p) | (b,n,p) <- codeIndex, elem b tilltype]
formatBill :: BillType -> String
formatBill billtype = title ++ "\n\n" ++ content ++ "\n\n" ++ foot ++ "\n"
  where nLeft n = toInteger (lineLength-(length n))
        title_name = "Haskell Stores" 
        title = pushRight title_name ((div (nLeft title_name) 2)+(toInteger (length title_name))) " "
        foot = formatTotal (makeTotal billtype)
        content = formatLines billtype

produceBill :: TillType -> String
produceBill = formatBill . makeBill

lineLength :: Int
lineLength = 30

formatPence :: Price -> String
formatPence pence = show ((fromIntegral pence)/100)

formatLine :: (ItemName, Price) -> String
formatLine (n,p) = n ++ (pushRight (formatPence p) (nLeft n) ".")
  where nLeft n = toInteger (lineLength-(length n))

formatLines :: BillType -> String
formatLines lines = onSeparateLines [formatLine line | line <- lines]

makeTotal :: BillType -> Price
makeTotal billtype = sum [p | (_,p) <- billtype]

formatTotal :: Price -> String
formatTotal price = formatLine ("Total", price)
