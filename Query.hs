-- SERBOI FLOREA-DAN 325CB
-- Pentru testul 3 de la bonus obtin un timp de rulare de aproximativ 2 minute
-- Pentru testul 4 de la bonus obtin un timp de rulare de aproximativ 3 ore
-- Pentru testele 1 si 2 de la bonus obtin un timp foarte mare de rulare
-- Pentru toate celelalte cerinte(teste) timpul de rulare este mic
module Query where

import UserInfo
import Rating
import Movie
import Data.List
import Numeric

type Column = String
type TableSchema = [Column]
type Field = String
type Entry = [Field]

data Table = Table TableSchema [Entry]

type ColSeparator = Char
type LnSeparator = Char

-- functia parseaza tabelul
parse_table :: Char -> Char -> String -> [[String]]
parse_table colsep linesep = (map (splitBy colsep)) . (splitBy linesep)
        where   splitBy :: Char -> String -> [String]
                splitBy c = foldr op [] 
                            where
                              op x [] 
                                 | x /= c = [[x]]
                                 | otherwise = [[]]
                              op x (y:ys)
                                 | x /= c = (x:y):ys
                                 | otherwise = []:(y:ys)
-- functia calculeaza table schema
t_sch :: ColSeparator -> LnSeparator -> String -> [Column]
t_sch colsep linesep table = head (parse_table colsep linesep table)
-- functia calculeaza intrarile
entries :: ColSeparator -> LnSeparator -> String -> [Entry]
entries colsep linesep table = tail (parse_table colsep linesep table)
-- functia citeste un tabel
read_table :: ColSeparator -> LnSeparator -> String -> Table
read_table csep lnsep table = Table (t_sch csep lnsep table) (entries csep lnsep table)
-- citim fiecare tabel in parte
user_info = read_table '|' '\n' UserInfo.user_info_str
rating = read_table ' ' '\n' Rating.rating_str
movie = read_table '|' '\n' Movie.movie_str
-- functia calculeaza cuvintele cu lungime maxima de pe fiecare coloana
-- (fara table schema, doar intrarile)
max_length_per_column :: [[String]] -> [Int]
max_length_per_column ([]:_) = []
max_length_per_column table = 
    ((maximum(map length (map head table))):(max_length_per_column (map tail table)))
-- functia compara elementele de pe aceeasi pozitie din 2 liste diferite 
-- si pune maximul in lista rezultat
maxbeetween2 :: [Int] -> [Int] -> [Int]
maxbeetween2 [] [] = []
maxbeetween2 (x:xs) (y:ys) = ((max x y):(maxbeetween2 xs ys))
-- functia calculeaza cuvintele cu lungime maxima de pe fiecare coloana(cu tot cu table schema)
table_max_length_per_column :: Table -> [Int]
table_max_length_per_column (Table header entries) = maxbeetween2 (map length header) 
 (max_length_per_column entries)
-- functia calculeaza numarul de caractere care apare fiecare linie(fara '\n')
no_characters_per_line :: [Int] -> Int
no_characters_per_line lengths = foldl' (+) ((length lengths)+1) lengths 
-- functia genereaza un padding de o lungime dorita cu un anume caracter
gen_padding :: Int -> Char -> String
gen_padding 0 c = ""
gen_padding l c = (c: (gen_padding (l-1) c))
-- functia afiseaza formatat, dupa modul dorit, o intrare
show_entry :: [Int] -> Entry -> String
show_entry [] [] = "|\n"
show_entry (l:ls) (f:fs) = "|"++f++(gen_padding (l - (length f)) ' ')++(show_entry ls fs)
-- functia afiseaza toate intrarile dupa modul dorit
show_entries :: [Int] -> [Entry] -> String
show_entries l [] = ""
show_entries l (e:es) = (show_entry l e)++(show_entries l es)

-- inrolam Table in clasa Show, implementam show dupa modul dorit
instance Show Table where
 show (Table header entries) = 
  (gen_padding (no_characters_per_line (table_max_length_per_column (Table header entries))) '-')
   ++"\n"
   ++(show_entry (table_max_length_per_column (Table header entries)) header)++
  (gen_padding (no_characters_per_line (table_max_length_per_column (Table header entries))) '-')
   ++"\n"
   ++(show_entries (table_max_length_per_column (Table header entries)) entries)++
  (gen_padding (no_characters_per_line (table_max_length_per_column (Table header entries))) '-')
   ++"\n"

data FilterCondition = Lt Field Integer | Eq Field String | In Field [String] | Not FilterCondition

-- functia converteste un String la un Integer
rd_int :: String -> Integer
rd_int x = read x :: Integer

-- functia filtreaza o intrare dupa conditia de filtrare
getFilter :: FilterCondition -> TableSchema -> (Entry -> Bool)
getFilter (Lt field integer) t_sch entry = (rd_int (entry !! (indexOf field t_sch))) < integer
getFilter (Eq field string) t_sch entry = (entry !! (indexOf field t_sch)) == string
getFilter (In field strings) t_sch entry = (entry !! (indexOf field t_sch)) `elem` strings
getFilter (Not filterCond) t_sch entry = not (getFilter filterCond t_sch entry)

data Query = Filter FilterCondition Query |  
             Select [String] Query |
             SelectLimit [String] Integer Query |
             Cosine Query |
             Query :|| Query |
             Atom Table

-- functia returneaza coloanele dorite din tabel(fara t_sch) asa cum apar ordonate in tabel
select_fields :: [Field] -> Entry -> [Entry] -> [Entry]
select_fields fields [] x = x
select_fields fields t_sch entries
      | (head t_sch) `elem` fields = (my_concat (map (take 1) entries) 
        (select_fields fields (tail t_sch) (map (drop 1) entries)))
      | otherwise = (select_fields fields (tail t_sch) (map (drop 1) entries))
-- functia concateaza elementele a 2 liste(care sunt tot liste), 
-- de pe aceeasi pozitie, in lista rezultat
my_concat :: [[a]] -> [[a]] -> [[a]]
my_concat x y = zipWith (++) x y

-- functia returneaza field-urile dorite din table schema asa cum apar ordonate in tabel
fields_from_table_schema :: [Field] -> Entry -> Entry
fields_from_table_schema fields [] = []
fields_from_table_schema fields t_sch 
      | (head t_sch) `elem` fields = ((head t_sch):(fields_from_table_schema fields (tail t_sch)))
      | otherwise = (fields_from_table_schema fields (tail t_sch))
-- functia intoarce index-ul elementului din lista
indexOf :: Eq a => a -> [a] -> Int
indexOf x [] = -1
indexOf x (y:ys)
          | (x == y) = 0
          | otherwise = 1 + (indexOf x ys)

-- functia extrage coloana cu un anume index din tabel(indexarea incepe de la 0)
extract_col :: Int -> [Entry] -> Entry
extract_col n = map (head . drop n)

-- functia ia fiecare element dintr-o lista si creeaza o noua lista 
-- in care fiecare element e o lista cu un singur element si anume elementul din lista initiala
make_list_every_elem :: [a] -> [[a]]
make_list_every_elem [] = []
make_list_every_elem (x:xs) = ([x]:make_list_every_elem xs)

-- functia selecteaza doar coloanele specificate din tabel
ord_select :: [Field] -> Entry -> [Entry] -> [Entry]
ord_select fields t_sch table
           | (length fields) > 1 = (my_concat (make_list_every_elem (extract_col 
            (indexOf (head fields) t_sch) table)) (ord_select (tail fields) t_sch table))
           | otherwise = (make_list_every_elem (extract_col (indexOf (head fields) t_sch) table))

eval :: Query -> Table
eval (Atom table) = table
eval (Filter filterCond query) = (Table t_sch (filter (getFilter filterCond t_sch) entries)) 
                                 where (Table t_sch entries) = eval query
eval (Select fields query) = (Table fields (ord_select fields t_sch entries))
                                 where (Table t_sch entries) = eval query
eval (SelectLimit fields n query) = 
    (Table fields (take (fromIntegral n) (ord_select fields t_sch entries)))
                                 where (Table t_sch entries) = eval query
eval (query1 :|| query2) = (Table table_schema1 (entries1++entries2))
                                 where (Table table_schema1 entries1) = eval query1
                                       (Table table_schema2 entries2) = eval query2

eval (Cosine query) = (calc_sim (sort_table(Table t_sch entries)) sim_table)
	                  where (Table t_sch entries) = eval query

-- functia forteaza un numar zecimal sa aiba 4 zecimale si il converteste la un string
show_float :: RealFloat a => a -> String
show_float x = showFFloat (Just 4) x ""
-- functia sorteaza un tabel dupa user_id(care reprezinta prima coloana din tabel)
sort_table :: Table -> Table
sort_table (Table t_sch entries) = (Table t_sch (sort entries))

-- functia calculeaza x1^2 + x2^2 + ... + xn^2, unde xi sunt rating-urile date
pows_sum :: Table -> Integer
pows_sum (Table t_sch []) = 0
pows_sum (Table t_sch entries) = 
 (rd_int((head entries) !! 2))*(rd_int((head entries) !! 2)) + pows_sum (Table t_sch (tail entries))

-- norma euclidiana(norma 2)
norm :: Table -> Float
norm table = sqrt (fromIntegral (pows_sum table))

-- este suficient sa parcurgem notele date de un singur user
-- (daca unul dintre ei a vazut / notat mai multe filme, nu conteaza, deoarece rezultatul e 0)
-- si sa facem suma produselor notelor pentru filmele vazute de ambii
-- trebuie sa facem rost de index-ul intrarii in lista intrarilor celui de-al doilea user 
-- pentru filmul curent(care are index-ul 1 in intrare)
-- si de acolo extragem nota(care are index-ul 2 in intrare)
-- primul caz: filmul a fost notat de amandoi: efectuam inmultirea notelor
-- altfel: nota se considera 0 pt cel care nu a vazut filmul, 
-- deci nu mai este necesara nicio inmultire
prod_sum :: Table -> Table -> Integer
prod_sum (Table table_schema1 []) (Table table_schema2 []) = 0
prod_sum (Table table_schema1 []) (Table table_schema2 entries2) = 0
prod_sum (Table table_schema1 entries1) (Table table_schema2 []) = 0
prod_sum (Table table_schema1 entries1) (Table table_schema2 entries2)
       | ((head entries1) !! 1) `elem` (map (!! 1) entries2) = (rd_int ((head entries1) !! 2)) *
       (rd_int ((entries2 !! (indexOf ((head entries1) !! 1) (map (!! 1) entries2))) !! 2)) + 
       (prod_sum (Table table_schema1 (tail entries1)) (Table table_schema2 entries2) )
       | otherwise = (prod_sum (Table table_schema1 (tail entries1)) (Table table_schema2 entries2))

-- functia calculeaza similaritatea a 2 useri
similarity :: Table -> Table -> Float
similarity table1 table2 = (fromIntegral (prod_sum table1 table2)) / ((norm table1) * (norm table2))

-- functia returneaza tabelul incepand cu user-ul urmator 
-- sau tabel cu lista vida daca nu mai exista user
next_user :: Table -> Table
next_user (Table t_sch (e:entries))
         | (length entries) == 0 = (Table t_sch [])
         | (e !! 0) /= ((head entries) !! 0) = (Table t_sch entries)
         | otherwise = next_user (Table t_sch entries)

-- functia calculeaza similaritatea dintre un user si ceilalti user 
-- si adauga intrarile in tabelul de similaritate

calc_user_sims :: Table -> Table -> Table -> Table
calc_user_sims (Table user_t_sch user_entries) (Table t_sch []) (Table res_t_sch res_entries) =
    (Table res_t_sch res_entries)
calc_user_sims (Table user_t_sch user_entries) (Table t_sch entries) (Table res_t_sch res_entries) =
    calc_user_sims (Table user_t_sch user_entries) (next_user(Table t_sch entries)) 
    (Table res_t_sch (res_entries++[[(head (head user_entries)), (head (head entries)), 
        show_float (similarity (Table user_t_sch user_entries) 
            (eval $ Filter (Eq "user_id" (head (head entries))) $ Atom (Table t_sch entries)))]]))

-- acesta este tabelul de similaritate a userilor
sim_table = (Table ["user_id1", "user_id2", "sim"] [])
-- functia returneaza intrarile unui tabel
get_entries :: Table -> [Entry]
get_entries (Table t_sch entries) = entries
-- functia calculeaza similaritatile dintre utilizatori
-- si pune rezultatele in tabelul de similaritate
calc_sim :: Table -> Table -> Table
calc_sim (Table t_sch entries) (Table res_t_sch res_entries) 
	| get_entries (next_user (Table t_sch entries)) == [] = 
    (calc_user_sims (eval $ Filter (Eq "user_id" (head (head entries))) 
    $ Atom (Table t_sch entries)) (next_user (Table t_sch entries)) (Table res_t_sch res_entries))
	| otherwise = calc_sim (next_user (Table t_sch entries)) 
    (calc_user_sims 
    (eval $ Filter (Eq "user_id" (head (head entries))) $ Atom (Table t_sch entries)) 
    (next_user (Table t_sch entries)) (Table res_t_sch res_entries))

--functia selecteaza câmpurile ​user_id și ​ocupație din tabela user_info cu proprietatea că toți
--utilizatorii selectați sunt din aceeași zona cu un user identificat prin ​user_id primit ca
--parametru.
same_zone :: String -> Query
same_zone user_id = Atom (eval $ Filter (Not (Eq "user_id" user_id)) 
    $ Select ["user_id", "occupation"] $ Filter (Eq "zone" (head (head entries))) $ Atom user_info)
    where 
    (Table t_sch entries) = eval $ Select ["zone"] $ Filter (Eq "user_id" user_id) $ Atom user_info

--functia selecteaza campurile ​ocupație și zona din tabela ​user_info cu proprietatea că intrările 
--din tabela descriu persoane cu o vârstă mai mare strict de ​x ani și mai mică strict de y ani,
--de sex masculin.
male_within_age :: Integer -> Integer -> Query
male_within_age x y = Atom(eval $ Select ["occupation", "zone"] $ Filter (Not (Eq "age" (show x))) 
    $ Filter (Not (Lt "age" x)) $ Filter (Lt "age" y) $ Filter (Eq "sex" "M") $ Atom user_info)

--functia selecteaza campurile user_id din tabela ​user_info cu proprietatea că intrările din tabelă
--descriu persoane care sunt dintr-o lista de zone și cu ocupația dintr-o lista de ocupații
--primite ca parametri, toți cu o vârstă mai mică strict decât un threshold primit ca
--parametru.
mixed :: [String] -> [String] -> Int -> Query
mixed zones occupations max_age = Atom(eval $ Select ["user_id"] 
    $ Filter (In "occupation" occupations) $ Filter (In "zone" zones) 
    $ Filter (Lt "age" (fromIntegral max_age)) $ Atom user_info)
