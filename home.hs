import System.IO
import Numeric
import System.Process

my_filter::[(Double,Double)]->Int->Int->[(Double,Double)]
my_filter [] _ _ = []
my_filter ((t,y):xs) i j | mod i j == 0 = (fromIntegral(round (1000.0*t))/1000,fromIntegral(round (1000.0*y))/1000) : my_filter xs (1+i) j
                         | otherwise = my_filter xs (1+i) j

ei :: (Double,Double)->Double-> (Double,Double)
ei  (t, y) t'  = (t', y + h*f t y)
        where   h  = t' - t
                f t y =sin(t*y)-exp(0.1*t)*y

rk :: (Double,Double)->Double-> (Double,Double)
rk   (t, y) t'  = (t', y + h*(k1 + 2.0*k2 + 2.0*k3 + k4)/6.0)
        where
            h  = t' - t
            k1 = f t y
            k2 = f (t + 0.5*h) (y + 0.5*h*k1)
            k3 = f (t + 0.5*h) (y + 0.5*h*k2)
            k4 = f (t + 1.0*h) (y + 1.0*h*k3)
            f t y =sin(t*y)-exp(0.1*t)*y


a=0.0; b=10.0;  y0=1;  n=1000; h1=0.001; h=(b-a)/fromIntegral(n)

testEiler a b h1=scanl ei  (0.0, (1.0)) [a+h1,a+2.0*h1..b]
t_1= testEiler a b h1
t_1_f= my_filter t_1 0 100

testRk4  a b h=scanl rk  (a, y0) [a+h,a+2.0*h..b]
t_2= testRk4 a b h
t_2_f = my_filter t_2 0 10

t_1_1_f=my_filter t_2 0 10
main = do
  writeFile "ei100.txt" (show t_1_f)
  writeFile "rk100.txt" (show t_2_f)
  system "grep -l '(' *100*.txt | xargs sed -e 's/[\\|[\\|]\\|(//g' -i"
  system "grep -l ')' *100*.txt | xargs sed -e '0,/A/ s///;s/,/ /g;s/)/\\n/g' -i"
  system "grep -l ' ' *100*.txt | xargs sed -e 's/^[ \t]*//;$d' -i"
  writeFile "conf.gnu" ("plot \"ei100.txt\" using 1:2 w l title \"Эйлер\",\"rk100.txt\" using 1:2 w l title \"Крунт\"\npause -1 ")
  system "gnuplot conf.gnu"

--  system "grep -l ')' *100*.txt | xargs perl -pi -e 's/)/]/g' ei100.txt"
