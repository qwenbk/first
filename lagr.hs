import System.Process
import System.Environment

a=0; b=6; hh=0.05
mx=[0, 0.5, 1, 2, 3.5, 4, 6]
my=[-2.371, -4.592, -6.937, -3.297, 2.895, 7.981, 11.625]

--------p---------x--------mx(j)------ mx(k)-----pp
pp :: Double -> Double -> Double -> Double -> Double
pp p x mxj mxk = p * (x - mxj)/(mxk - mxj)

------------- arr_x------j---------b---------x---------P--------k------res
cycle_1 :: [Double] -> Int -> Int -> Double -> Double -> Int -> Double
cycle_1 arr_x j b x p k
 | (j < b) && (j /= k) = cycle_1 arr_x (j + 1) b x (pp p x (arr_x !! j) (arr_x !! k) ) k
 | (j == k) = cycle_1 arr_x (j + 1) b x p  k
 | (j >= b) = p

------------- arr_x--------arr_y-------k--------n----------l--------x-----------res
lagrange ::  [Double] -> [Double] -> Int -> Int -> Double -> Double -> Double

lagrange arr_x arr_y k n l x
 |  (k < n) = lagrange arr_x arr_y (k+1) n  (l + ((cycle_1 arr_x 0 n x 1 k)* ((arr_y !! k)))) x
 |  (k == n) = l

{-	L = 0 ' В этой переменной накапливаем сумму (2.7)
For k = 0 To N
 P = 1 'Здесь в цикле накапливаем произведение
 For j = 0 To N
 If j <> k Then P = P * (x − mx(j)) / (mx(k) − mx(j))
 Next j
 ' В переменной L накапливаем очередное слагаемое
 L = L + my(k) * P
Next k
Лагранж = L -}

make_string :: [Double] -> [Double] -> Double-> String
make_string  x y n = make_string' x y "" (n) 0

make_string' :: [Double] -> [Double] -> String -> Double -> Double -> String
make_string' x y str n q
 | q <= n = make_string' (tail x) (tail y)  (str ++ show (head x) ++ " " ++ show (head y) ++ " " ++ " \n" ) n (q + 1)
 | q >n = str

make_x :: Double -> Double -> Double -> [Double]
make_x a b step = [a,a+step.. b]

test2 = make_x a b hh


lag :: [Double] -> [Double] -> [Double] -> Int -> Int -> Double -> [Double]
lag mx arr_x arr_y k n l = [ lagrange arr_x arr_y k n l x |  x <- mx]

test3 = lag test2 mx my  0 7 0

prov=zipWith (++)(map (++ " ")(map(show)mx)) (map(++ "\n")(map(show)my))
test10 = make_string test2 test3 (fromIntegral (length (test2) -1))

my_Draw =
 writeFile "points.txt" (test10)
 >> writeFile "point1.txt" (foldl1(++)prov )
 >> writeFile "conf.gnu" ("set grid\nset xrange [0:6]\nplot \"points.txt\" using 1:2 w l title \"Лагранж\",\"point1.txt\" using 1:2 with points pointtype 5 title \"Лагранж по точкам\" \npause -1 ")
 >> system "gnuplot conf.gnu"

main = my_Draw
