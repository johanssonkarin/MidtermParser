{- 2018-11-02 -}

{-
    Grading a mid-term with Haskell

    The intension of this lab exercise is to provide
    extended experience with programming with Haskell.
    You are also strongly encouraged to use higher-order
    functions where possible.

    The script is built for the following assumptions:
    - The whole question paper contains N questions.
    - N questions are divided into G number of groups (of questions).
    - There are S number of shuffles of the groups so that each
      shuffle (also called set) may have a different order of the groups.
    - Order of the questions in a particular group (of questions) is always
      unchanged.

    The executable is expected to read two files cs2104mid.txt and cs2104mid.csv

    cs2104mid.txt contains the list of G groups' identifier (0,...,G-1) in the 
    original order with the number of questions in it.
    Total number of questions should be the same as N.
    It is followed by the S number of sets with the set identifier and the 
    order of the groups in it.
    This is followed by the set of correct answers for each of the
    questions in the original order. Take note that each question
    may accept one or more answers as the correct answer.

    Each correct answer will carry 1 mark, while incorrect answers
    will be penalized.

    EBNF of it is as follows:
    Z ::= A '\n' B '\n' Q
    P ::= '(' C ',' C ')'
    R ::= '(' D D ',' '[' C {',' C} ']' ')'
    A ::= '[' P { ',' P } ']'
    B ::= '[' R { ',' R } ']'
    C ::= {0|1|...|9}+
    D ::= 'A'|'B'|'C'|'D'|'E'
    F ::= '{' D {',' D } '}'
    Q ::= '[' F {',' F} ']'

    Intuitively,
    - (x1,x2) is a pair of group number (x1) and number of questions in it (x2),
    - [x1,x2] is a list of the group numbers x1 and x2 that denotes the ordering of the groups for a particular set and it is different from [x2,x1]
    - {x1,x2} is a set of x1 and x2 that denotes the set of correct answers of a particular question in order and it is the same as {x2,x1}.

    cs2104mid.csv contains answers of all the exam participants.
    In each row, the participant's ID, a number that can be ignored,
    the set identifier (two characters separated by a comma (','),
    and the answers of the N questions are given.
    Its EBNF is given below. Note that the EBNF given above is extended here.
    X ::= Y {'\n' Y}
    Y ::= I ',' C ',' D { ',' G}
    G ::= [D | '*']
    I ::= {'A'|...|'Z'|'0'|...|'9'|' '}+

    Example: Total questions: 10, Number of groups, G: 3, and
    the number of sets, S: 3. Groups are 1, 2, and 3 their total
    number of questions are 3, 4, and 3 respectively. The shuffles (or sets)
    are identified by AA, BC, or CA. In the set BC, the three groups are permuted as
    2,3,1. The files are given below.

    Sample cs2104mid.txt
    ------------
    [(1,3), (2,4), (3,3)]
    [(AA,[1,2,3]), (BC,[2,3,1]), (CA,[3,1,2])]
    [{A},{B},{C,E},{C},{C},{D},{A},{D},{A},{C}]

    Sample cs2104mid.csv
    ------------
    A000001R,12,C,A,A,B,C,C,C,D,A,D,A,C
    A 00001R,12,B,C,A,,C,C,*,D,A,D,A,C

    Please note that the accompanied file 'mcq_0.pdf' is providing the reference order of the questions.
-}

import Data.List
import Text.Parsec
import Data.Function (on)

type Groups = [(Int,Int)]

type Permutations = [(String, [Int])]

type CorrectAnswers = [[Char]]

type Format = (Groups, Permutations, CorrectAnswers)

type GivenAnswers = [(String, Int, String, [Char])]

type SParsec = Parsec String ()

main = do
          d  <- read_csv
          z <- format_answers d
          grade_quiz z



read_file :: FilePath -> IO [String]
read_file x =
  do
    content <- readFile x
    let llcontent = lines content
        lz = length llcontent
     in do
          return llcontent

whitespaces :: SParsec [Char]
whitespaces = many (tab <|> space)

comma = whitespaces >> char ',' >> whitespaces

parseF = do a <- parseA
            whitespaces
            b <- parseB
            whitespaces
            q <- parseQ
            whitespaces
            eof
            return (a, b, q)

parseSqBr :: SParsec a -> SParsec a
parseSqBr x = between (char '[') (char ']') x

parseParen :: SParsec a -> SParsec a
parseParen x = between (char '(') (char ')') x

parseCrBr :: SParsec a -> SParsec a
parseCrBr x = between (char '{') (char '}') x

commaSep p  = p `sepBy` comma

parseA :: SParsec Groups
parseA = parseSqBr parseA'

parseA' :: SParsec [(Int, Int)]
parseA' = commaSep $ parseParen parseA''

parseA'' :: SParsec (Int, Int)
parseA'' = do c1 <- parseC
              comma
              c2 <- parseC
              return (c1,c2)

parseB :: SParsec Permutations
parseB = parseSqBr parseB'

parseB' :: SParsec [(String, [Int])]
parseB' = commaSep $ parseParen parseB''

parseB'' :: SParsec (String, [Int])
parseB'' = do d1 <- parseD
              d2 <- parseD
              comma
              e <- parseE
              return ([d1,d2], e)

parseC :: SParsec Int
parseC = parsecMap read $ many1 digit

parseD = char 'A' <|> char 'B' <|> char 'C' <|> char 'D' <|> char 'E'

parseE = parseSqBr parseE'

parseE' = commaSep parseC

parseQ :: SParsec CorrectAnswers
parseQ = parseSqBr $ commaSep $ parseCrBr $ commaSep parseD


{-
   F ::= A '\n' B '\n' Q
   A ::= '[' { [','] '(' C ',' C ')' } ']'
   B ::= '[' { [','] '(' D D ',' E ')' } ']'
   C ::= {0|1|...|9}+
   D ::= 'A'|'B'|'C'|'D'|'E'
   E ::= '[' { [','] C } ']'
   Q ::= { '{' { D [',']} '}' [','] }
-}

read_format =
  do 
    ff <- readFile "cs2104mid.txt"
    case parse parseF "" ff of
       Left e  -> error "Error parsing input:"
       Right r -> return r 


format_answers answersOfAll =
    do
        format <- read_format
        let groups = get1st format
            permutations = get2nd format
            corrAns = ["*"] ++ get3rd format -- empty string to make list of correct answers same length as student answers (i.e. empty instead of a student ID)
          in return $ foldr (\ans acc-> acc ++ [shuffle_answers groups permutations ans]) [corrAns] answersOfAll 
        

shuffle_answers groups permutations answers =
  let studentID =  head answers
      pt = (answers !! 2) ++ (answers !! 3)
      as = drop 4 answers
      order = head [z | (y,z) <- permutations, y == pt]
   in [studentID] ++ perm_answers order as groups


perm_answers order answers groups =
    let aux el al newList =
          case el of
            []       -> concat $ snd $ unzip (sortBy (compare `on` fst) newList)
            x:xs     -> let elToDropAndTake = head [z | (y,z) <- groups, y==x]
                             in aux xs (drop elToDropAndTake al) (newList ++ [(x, (take elToDropAndTake al))])
     in  aux order answers []

     
extract_data all_lines =
   return $ map parseLine all_lines

parseLine a_line =
    let res = parse (whitespaces >> (commaSep $ many (alphaNum <|> char '*' <|> space))) "" a_line
    in  case res of
        Right (a:_:b1:b2:c) -> (a, b1++b2, c)
        _ -> error "Parse Error"

grade_quiz z =
  do
      print_file z
      putStrLn(" ")
      print_score (sort_student (get_score z)) 0
      putStrLn(" ")
      print_score (sort_mark (get_score z)) 1
      putStrLn(" ")
      print_stats (sort_mark (get_score z))
      putStrLn(" ")

print_file z =
  do 
    putStrLn("Given Answers: ")
    dd <- read_file "cs2104mid.csv"
    let aa = reverse $ map (\a -> intercalate ", " a) z
      in foldr (\ a m -> putStrLn a >> m) (return ()) aa
    putStrLn(" ")

print_score :: [(String, Int)] -> Int -> IO ()
print_score ll x =
  do 
    if (x == 0)
      then putStrLn ("Score (Sorted by ID):" )
      else putStrLn ("Score (Sorted by Score):" )
    foldr (\ (a,b) m -> putStrLn ( a ++", " ++ show b) >> m) (return ()) ll


get1st (a,_,_) = a
get2nd (_,a,_) = a
get3rd (_,_,a) = a

--compares and returns number of matching elements in lists
get_marks l1 l2 = sum $ map (\(x,y) -> if check_mark x y then 1 else 0) $ zip l1 l2
                     

check_mark x y =
    case y of
        []     -> False
        (z:zs) -> if x == y then True else check_mark x zs

--returns a list of tuples with each student's id and marks
get_score l = 
  foldr (\a acc -> acc ++ [(head a, (get_marks (head l) a))]) [] (tail l)

-- sorting the tuple list by mark and student id
sort_mark l = reverse (sortBy (compare `on` snd) l) --reverse to get decreasing order
sort_student l = sortBy (compare `on` fst) l

-- takes a sorted list and outputs maximum, minimum, average and median.
print_stats ::[(String, Int)] -> IO ()
print_stats ll =  
    let x = snd (unzip ll)
        s = sum x
        len = length x
        max = maximum x
        min = minimum x
        med = if odd len 
                then realToFrac (x !! (div len 2))
                else realToFrac ((x !! ((div len 2)-1)) + (x !! (div len 2)))/(fromIntegral 2)
        ave =  realToFrac s / (fromIntegral len)
     in putStrLn $ "Maximum: "  ++ show max ++ 
                 ", Minimum: " ++ show min ++
                 ", Median: " ++ show med ++
                 ", Average: " ++ show ave ++ ". "
              

csvFile = endBy line eol
line = sepBy1 cell (char ',')
cell = many (noneOf ",\r")
eol =  Text.Parsec.crlf <|> (char '\n')

read_csv :: IO [[String]]
read_csv =
  do c <- readFile "cs2104mid.csv"
     case parse csvFile "" c of
       Left e  -> error "Error parsing input:"
       Right r -> return r

{- 
the 'format'. output from read_format function
([(1,2),(2,1),(3,2),(4,3),(5,2),(6,4),(7,4),(8,3),(9,6),(10,4),(11,4)],
[("AA",[1,3,5,7,9,11,4,8,2,10,6]),
 ("AB",[2,4,6,8,10,1,5,9,3,11,7]),
 ("AC",[3,5,7,9,11,2,6,10,4,1,8]),
 ("AD",[4,6,8,10,1,3,7,11,5,2,9]),
 ("BA",[5,7,9,11,2,4,8,1,6,3,10]),
 ("BB",[6,8,10,1,3,5,9,2,7,4,11]),
 ("BC",[7,9,11,2,4,6,10,3,8,5,1]),
 ("BD",[8,10,1,3,5,7,11,4,9,6,2]),
 ("CA",[9,11,2,4,6,8,1,5,10,7,3]),
 ("CB",[10,1,3,5,7,9,2,6,11,8,4])],
["C","C","A","D","E","C","A","C","C","C","D","E","B","A","D","E","E","A","B","BE","C","C","D","B","E","D","B","C","B","D","A","C","E","ACD","B"])
-}
{-
:{
corrAns = ["C","B","D","B","C","C","A","A","C","C","D","E","E","A","A","C","B","E","A","B","A","D","C","B","A","C","E","E","B","C","C","D","B","E","C","","","","","","","","","","","","",""]
groups = [(1,2),(2,1),(3,2),(4,3),(5,2),(6,4),(7,4),(8,3),(9,6),(10,4),(11,4)]
order = [1,3,5,7,9,11,4,8,2,10,6]
data = [("A0149959A","AA",["E","C","B","C","A","C","D","E","E","A","A","E","B","E","D","B","A","E","C","B","A","A","C","B","B","A","A","E","B","D","E","D","C","B","A","","","","","","","","","","","","",""]),
("A0139633B","AA",["C","C","B","E","B","C","D","E","B","A","E","B","B","B","B","B","E","E","D","E","B","B","C","B","E","A","C","E","B","B","A","C","C","A","A","","","","","","","","","","","","",""])]
:}
-}