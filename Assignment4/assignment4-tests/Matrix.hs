-- $Id: MatrixAdd.hs,v 1.1 2013/09/24 14:54:21 leavens Exp $

module Matrix (Matrix, fillWith, fromRule, numRows, numColumns, at, mtranspose, mmap) where
newtype Matrix a = Mat ((Int,Int), (Int,Int) -> a)

fillWith :: (Int,Int) -> a -> (Matrix a)
fromRule :: (Int,Int) -> ((Int,Int) -> a) -> (Matrix a)
numRows :: (Matrix a) -> Int
numColumns :: (Matrix a) -> Int
at :: (Matrix a) -> (Int, Int) -> a
mtranspose :: (Matrix a) -> (Matrix a)
mmap :: (a -> b) -> (Matrix a) -> (Matrix b)


fillWith (m , n) e = Mat ((m , n), (\(_,_) -> e))

fromRule (m , n) f = Mat ((m , n), f)

numRows (Mat ((m,_),_)) = m

numColumns (Mat ((_,n),_)) = n

at (Mat ((m , n), f)) (i,j) | ((i >= 1) && (i <= n) || (j >= 1) && (j <= n)) = f (i,j)

mtranspose (Mat ((m , n),f)) = (Mat ((n , m), (\(j,i) -> f (i,j))))

mmap h (Mat ((m , n),f)) = (Mat ((m , n), (h.f) ))