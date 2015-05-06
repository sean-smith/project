[
1(  Just [], Just [([],Set [5]),([],Set [5])])
2,( Just [], Just [([],Set [5]),([],Set [5])])
3,( Just [], Just [([],Set [-30]),([],Set [-30])])
4,( Just [], Just [([],Set [9]),([],Set [9])])
5,( Just [], Just [([],Set [9]),([],Set [9])])
6,( Just [], Just [([],Set [9]),([],Set [9])])
7,( Just [], Just [([],Set [-30]),([],Set [-30])])
8,( Just [], Just [([],Set [-30]),([],Set [-30])])
9,( Just [], Just [([],Set [-30]),([],Set [-30])])
10,( Just [], Just [([],Set [-30]),([],Set [-30])])
11,( Just [], Just [([],Set [-30]),([],Set [-30])])
12,( Just [], Just [([],Set [-30]),([],Set [-30])])
13,( Just [], Just [([],Number (-1)),([],Number (-1))])
14,( Just [], Just [([],Number (-1)),([],Number (-1))])
15,( Just [], Just [([],Number 54),([],Number 54)])
16,( Just [], Just [([],Number (-1)),([],Number (-1))])
17,( Just [], Just [([],Number 5),([],Number 5)])
18,( Just [], Just [([],Number 54),([],Number 54)])
]


typeCheck [] (Min DATA) [(["T","C","Y","a"],Number 5)]


twoWithSameKeyHelper :: (Ord a, Ord b, Eq a) => [(a,b)] -> [(a,b)] -> Maybe (a, b, b, [(a,b)])
twoWithSameKeyHelper ((k1,v1):(k2,v2):kvs) keep = if k1 == k2 then Just (k1, v1, v2, keep++kvs) else twoWithSameKeyHelper kvs (keep++[(k1,v1)]++[(k2,v2)])
twoWithSameKeyHelper [(k1,v1)] keep = Nothing
twoWithSameKeyHelper [] keep = Nothing