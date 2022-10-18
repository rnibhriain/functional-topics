
parallel_map :: (a -> b) -> [a] -> [b]


force :: [a] -> ()
force (x:xs) = x `pseq` force xs
force _ = ()