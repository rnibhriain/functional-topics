-- Question 1
-- my understanding of this function 
-- is that when there is only one element left in the array 
-- the action will be done and the function terminates
-- when there is other actions the current action is performed
-- and the rest of the actions in the array are performed recursively
f1 :: [IO a] -> IO a
f1 [x] = do x
f1 (x:xs) = do 
        x  
        f1 xs 



-- Question 2: this should print "hello"

-- Question 3: this should print "hi there"

main :: IO ()
main =  f1 actions   
        where actions = [putChar 'h', putChar 'e', putChar 'l', putChar 'l', putChar 'o']


-- Question 4
-- this function returns while the input is false
-- else it continues with the IO

while :: IO Bool -> IO ()
while action = do p <- action
                  if not p then  return ()
                       else while action

-- Question 5
-- this function returns each action in an array of actions
f2 :: [IO a] -> IO [a]
f2 [action] = do 
                value <- action
                return [value]
f2 (action:actions) = do 
                        value <- action
                        val2 <- f2 actions
                        return ( [value] ++ val2)


read10 :: IO String
read10 = f2 $ take 10 actions
          where actions = getChar : actions