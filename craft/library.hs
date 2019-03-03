-- @Author: ariesduanmu
-- @Date:   2019-03-03 17:04:57
-- @Last Modified by:   ariesduanmu
-- @Last Modified time: 2019-03-04 00:08:54

type Person = String
type Book = String

data Loan = Loan Person Book
type Database = [(Person, Book)]

exampleBase :: Database
exampleBase = [("Alice", "Tintin"), ("Anna", "Little Women"),
               ("Alice", "Asterix"), ("Rory", "Tintin")]

books :: Database -> Person -> [Book]
books databse person = [b |(p,b) <- databse, p==person]

borrowers :: Database -> Book -> [Person]
borrowers databse book = [p |(p,b) <- databse, b==book]

borrowed :: Database -> Book -> Bool
borrowed databse book = (length [b |(_,b) <- databse, b==book]) > 0

numBorrowed :: Database -> Person -> Int
numBorrowed databse person = length (books databse person)

makeLoan :: Database -> Person -> Book -> Database
makeLoan database person book = database ++ [(person, book)]

returnLoan :: Database -> Person -> Book -> Database
returnLoan databse person book = [pair |pair <- databse, pair /= (person,book)]