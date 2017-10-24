module DoesItCompile where

bigNum x = (^) 5 $ x
wahoo = bigNum $ 10

x = print
y = print "woohoo!"
z = x "hello world"

a = 12 + b
b = 10000 * c
c = 1