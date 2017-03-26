import Data.List
import Data.Function

-- sortNumeric lst = sort lst
data Person = Person { name :: String , age  :: Int } deriving Show

instance Ord Person where compare x y = compare (age x) (age y)

instance Eq Person where x == y = (age x) == (age y)

ps = [ Person "Saburou" 30
     , Person "Jiro" 20
     , Person "Tarou" 10
     , Person "Hanako" 30
     ]

func = sortBy (\x y -> compare (name x) (name y)) ps

