import Data.Char
import qualified Data.Map as Map

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

-- if always needs else, because if is a function that must "return" something
doubleSmallNumber x =
  if x > 100
    then x
    else x * 2 -- list comprehension with a predicate

boomBangs xs =
  [ if x < 10
    then "BOOM!"
    else "BANG!"
  | x <- xs
  , odd x
  ]

-- tuples can have more than 1 type, lists can't
-- Example combining tuples and list comprehensions
-- Find right triangle with integers for all sides, sides <= 10, perimeter 24
-- All triangles with integers <= 10 for sides
triangles = [(a, b, c) | c <- [1 .. 10], b <- [1 .. 10], a <- [1 .. 10]]

-- Right triangles of the above
rightTriangles =
  [ (a, b, c)
  | c <- [1 .. 10]
  , b <- [1 .. c]
  , a <- [1 .. b]
  , a ^ 2 + b ^ 2 == c ^ 2
  ]

-- Perimeter == 24 of the above
rightTriangles' =
  [ (a, b, c)
  | c <- [1 .. 10]
  , b <- [1 .. c]
  , a <- [1 .. b]
  , a ^ 2 + b ^ 2 == c ^ 2
  , a + b + c == 24
  ]

-- type declarations before functions
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A' .. 'Z']]

-- it wouldn't compile if i put this above it instead:
-- removeNonUppercase :: [Char] -> Char
-- if unsure, can open up intepreter and do :t function to find out type of
-- function. can actually do that w/ any expression.
-- let's test out a type declaration quick
circumference :: Double -> Double
circumference r = 2 * pi * r -- works with 4 and 4.0

-- type variables are specified without upper case to allow for polymorphic
-- functions
reverseCons :: [a] -> a -> [a]
reverseCons list a = list ++ [a]

-- typeclasses
-- example: "elem" function for testing if something is a member of list, :t:
-- (Eq a) => a -> [a] -> Bool
-- a can be anything that can be equal to another thing
-- sometimes, for something ambiguous, a type annotation is needed -
-- "4" is a string, and we can convert it into something readable (something in
-- typeclass Read)
-- however, ghci won't know what type that is unless we specify it, because
-- there are a lot of types in Read
-- so, you have to do this:
-- read "4" :: Int
-- or
-- read "4" :: Float
-- pattern matching, evaluated top to bottom
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

-- pattern matching with recursion. specific first, general later
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- always include a catch-all pattern, or your program will crash with
-- unexpected input
-- pattern matching with vector addition
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
-- need () to pattern match and bind to several variables
head' (x:_) = x

-- "as patterns" to keep reference to entire thing pattern matched
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- guards test whether property of a value, or several, are true or false
-- like many chained if-else statements
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= 18.5 = "underweight"
  | bmi <= 25.0 = "healthy"
  | bmi <= 30.0 = "overweight"
  | otherwise = "obese"
  where
    bmi = weight / height ^ 2

-- define vars at end of function with "where"
-- make your own max function
max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

-- can also do function definitions inline
-- Make your own compare function
marioCompare :: (Ord a) => a -> a -> Ordering
a `marioCompare` b
  | a > b = GT
  | a == b = EQ
  | otherwise = LT

-- initials
initials :: String -> String -> String
initials firstname lastname = [toUpper f] ++ ". " ++ [toUpper l] ++ "."
  where
    (f:_) = firstname
    (l:_) = lastname

-- can pattern match with "where", too
{-
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2
--}
-- "where" bindings bind variables at end of function. If you need something
-- more localized, i.e. something that won't span across guards, use "let"
-- bindings. "where" bindings span across the whole function, including across
-- guards.
-- see this used to calc a cylinder's surface area
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
   in sideArea + 2 * topArea

-- let <bindings> in <expression>
-- unlike "where" bindings, which are just syntactic sugar, "let" bindings are
-- expressions themselves
-- for comparison, see an if statement, which can be stuck in anywhere:
-- 4 * (if 10 > 5 then 10 else 0) + 2
-- you can do the same with a let binding:
-- 4 * (let a = 9 in a + 1) + 2
-- can use for fn in local scope:
-- [let square x = x * x in (square 5, square 3, square 2)]
-- use ; to separate several vars inline:
-- (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)
-- can also use with pattern matching, as you can with "where" bindings:
-- (let (a,b,c) = (1,2,3) in a+b+c) * 100
-- can also use in list comprehensions, this is pretty neat
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- why not always use a let binding? too local in scope; can't be used across
-- guards
-- case expressions: similar to cases in c/python, but also with pattern
-- matching,  and they're expressions so you can use them anywhere
-- syntax:
{-
case expression of pattern -> result
                   pattern -> result
                   pattern -> result
                   ...
--}
--
--
describeList :: [a] -> String
describeList xs =
  "The list is " ++
  case xs of
    [] -> "listless"
    [x] -> "lonely :("
    xs -> "crowded >:("

-- recursion
-- goes great w/ pattern matching
-- hey, it's quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
   in smallerSorted ++ [x] ++ biggerSorted

-- higher order fns take fn as parameter, return fn as values
--
-- every fn in haskell officially only takes 1 parameter. all fns we've defined
-- that take multiple parameters so far are called curried functions
--
-- example: "max 4 5" first creates a function that takes a parameter and
-- returns True if the given parameter is greater than 4
--
-- type of "max" can be written like this:
--
-- max :: (Ord a) => a -> (a -> a)
--
-- this allows you to get a *partially applied* function back when you call a
-- function with too few parameters
--
-- check this out and try to grok it again if you forget. "compare 100" returns
-- a function with type sig a -> ordering:
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

-- this works b/c compare function has a type signature of:
-- (Ord a) => a -> (a -> Ordering)
--
-- can also do with infix functions, this is called "sectioning":
--
divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

-- functions can take functions as params too!
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- () necessary because -> is right associative
-- using higher order programming for useful things! here's an implementation
-- of zipWith function. takes a function and two lists, joins the lists by
-- applying function between corresponding elements
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- another stdlib fn - flip
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where
    g x y = f y x

-- maps and filters
-- map applies a function to a list and gives you back a new list
-- map :: (a -> b) -> [a] -> [b]
--
-- check this out:
--
-- map (map (^2)) [[1,2],[3,4,5,6],[7,8]]
-- filter takes a predicate (fn returning boolean), a list, returns list
-- satisfying the predicate
-- filter :: (a -> Bool) -> [a] -> [a]
--
-- can use list comprehensions for all of these - which to use is decided on a
-- per-case basis
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999 ..])
  where
    p x = x `mod` 3829 == 0

-- takeWhile takes a predicate, list, keeps returning elem while predicate
-- holds true, doesn't return anymore after that
-- lambdas
-- anonymous function, if you need it only once. usually made just to pass to a
-- higher order function
--
-- to make a lambda:
-- \ followed by name of parameters, spaces between them, ->, then fn body
-- example:
-- \xs -> length xs > 15
--
-- you may be better off using a partially applied function instead a lot of
-- the time; saves space
-- only folds and horses
--
-- common pattern: match on (x:xs), perform some work on single element, and
-- have special case for empty list, using pattern matching
--
-- to encapsulate this pattern, folds are used to reduce lists of values to a
-- single value
--
-- fold takes binary function, starting value ("accumulator"), and a list to
-- fold
--
-- foldl - apply to starting value, head of list, produces new accumulator
-- and applies that to new head of the list, etc. until 1 value remains
--
-- example with sum
--
--sum' :: (Num a) => [a] -> a
--sum' xs = foldl (\acc x -> acc + x) 0 xs
-- example with currying (similar to partially applied functions; viewing
-- functions as list of functions applied 1 at a time)
sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

-- example with elem; check if something's an element of a list
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys =
  foldl
    (\acc x ->
       if x == y
         then True
         else acc)
    False
    ys

-- generally, you can write, instead of "foo a = bar b a", write "foo = bar b"
--
-- foldr is similar, but binary function takes current value as first param and
-- accumulator as second param instead
-- map example with foldr
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

-- ++ much more expensive than : so usually use right folds when building a new
-- list from a list. also, right folds work on infinite lists, left ones don't,
-- since with right fold you eventually reach beginning
--
-- foldl1 and foldr1 are the same as foldr and foldr but assume first and last
-- elements of list are starting values, so no need for explicit start.
-- however, won't work on empty list
-- fun reverse example:
reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

-- scanl and scanr (scanl1 and scanr1, too) are similar to foldl and foldr, but
-- they return all intermediate accumulator states in the form of a list, final
-- result at tail of list for scanl, head of list for scanr.
-- ^
-- useful for monitoring progressions of functions that may be implemented as a
-- fold. example/exercise: how many elements does it take for the sum of
-- square roots of natural numbers to exceed 1000?
sqrtSums :: Int
sqrtSums = length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1 ..]))) + 1

-- function application with $
-- normal fn application has a very high precedence, but $ has lowest
-- precedence. also, right-associative
--
-- example: you can write sum $ map sqrt [1..130] instead of sum (map sqrt
-- [1..130])
--
{-| $ also lets you treat function application like another function, like this!
-- cool
--ghci> map ($ 3) [(4+), (10*), (^2), sqrt]
        [7.0,30.0,9.0,1.7320508075688772]
-}
-- function composition f(g(x)) in math world is f . g in haskell world
-- right associative
-- useful example:
-- ghci> map (negate . sum . tail) [[1..5],[3..6],[1..7]]
-- when you write a function without rightmost argument, you write it in a
-- "point free style"
--
-- fn x = ceiling (negate (tan (cos (max 50 x))))
--
-- becomes
--
-- fn = ceiling . negate . tan . cos . max 50
--
-- can't just remove x on the right because of all the parentheses
-- modules
-- collections of related functions, types, and typeclasses. program is
-- collection of modules. main module loads up others and uses functions to do
-- something.
-- loosely coupled modules are better, can be reused elsewhere, makes code more
-- manageable
--
-- Prelude module imported by default - not full stdlib though
--
-- import <module name>
-- all fns in the module become available in global namespace
--
-- Data.List is a cool one. fn called nub weeds out duplicates
--
-- :m + Data.List Data.Map Data.Set
-- ^^ import in ghci
--
-- import Data.List (nub, sort)
-- ^ if you only need a few functions
--
-- import Data.List hiding (nub)
-- ^ everything except nub
--
-- import qualified Data.Map
-- ^ then you reference "filter" function as Data.Map.filter
--
-- import qualified Data.Map as M
-- M.filter
--
-- ** good to click through below to learn more about haskell. reading source
-- is a good way to learn haskell, too
-- haskell libs: http://www.haskell.org/ghc/docs/latest/html/libraries/
-- Hoogle - good haskell search engine
--
-- (book goes over Data.List example functions - intersperse, intercalate,
-- transpose, concat... cool way to add polynomials:
-- map sum $ transpose <list of list of polynomial coefficients>
-- )
--
-- foldl' and foldl1' == stricter versions of lazy incarnations
-- lazy folds don't update accumulator as folding happens; accumulator makes
-- promise that it will eventually produce result (a "thunk"). thus, you might
-- get stack overflow if you wait to compute too many values. to avoid this
-- problem, the strict versions actually do non-lazy computation on the
-- accumulator as the fold progresses
--
-- Exercise: try to remember what this does:
-- map (\l@(x:xs) -> (x,length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
--
-- isInfixOf, isPrefixOf, and isSuffixOf
--
-- "partition" returns pair of lists - first list satisfies predicate, second
-- does not. unlike span or break because those 2 functions finish at the first
-- predicate match, but paritition goes through the whole list
--
-- "find" takes a predicate, a list, and returns first element satisfying the
-- predicate, wrapped in a "Maybe" value. "Maybe" value can be "Just something"
-- or "Nothing", kind of like a list can be empty or list with some elements,
-- except with only 1 element
-- this can be useful when you're not sure that you'll find an element that
-- will satisfy a predicate
--
-- elemIndices, findIndex, findIndices
--
-- zip3, zip4, zipWith3, zipWith4, ... all the way up to 7, for zipping more
-- than 2 lists
--
-- lines, unlines, words, unwords, nub, delete (delete deletes only first
-- occurrence)
--
-- \\ is the list difference function - deletes each elem from right side list
-- once from left list, kinda like set difference
--
-- union goes over every elem in second list, adds to first if not in first
-- list, removes duplicates from second list
--
-- intersect is like set intersection
--
-- insert puts element in a sortable list just before the first thing greater
-- than or equal to it
--
-- Data.List has generics - genericLength, genericTake, genericDrop, etc.
-- for using Num typeclasses instead of Int as the originals do for historical
-- reasons
--
-- example where that is needed:
-- let xs = [1..6] in sum xs / genericLength xs
-- because can't / with Int, only with members of Fractional typeclass
--
-- nubBy, deleteBy, unionBy, intersectBy, and groupBy let you define your own
-- equality functions and use those instead of (==)
--
-- an interesting function from Data.Function
-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- f `on` g = \x y -> f (g x) (g y)
-- used a lot with the By functions, because it lets you do stuff like this
-- which is pretty readable:
--
-- groupBy ((==) `on` (> 0)) values
--
-- sortBy, insertBy, maximumBy, ... etc. also take a function that returns an
-- Ordering, so you can sort by that function instead of < or >
--
-- sortBy (compare `on` length) xs -- good example of `on` function
--
-- compare `on` length
--
-- compare :: Ord a => a -> a -> Ordering
-- length :: Foldable t => t a -> Int
--
-- so compare `on` length gives back something like this:
-- compare (length x) (length y)
-- which returns an Ordering, so you can give that to the sortBy function
-- next up, Data.Char module
--
-- tons of predicate Char -> Bool functions
-- isAscii, isPunctuation, ...
--
-- also exports datatype like Ordering, which is an enumeration:
--
-- GeneralCategory. use generalCategory function to retrieve this for a Char.
-- GeneralCategory type part of Eq typeclass, so can compare GeneralCategories
-- (Space, DecimalNumber, etc.)
--
-- TODO: I wonder if there's an extension of GeneralCategory for non-roman
-- character sets.
--
-- digitToInt, intToDigit, are also useful in Data.Char for hex it seems
--
-- ord/chr map characters to numbers
-- very elegant caesar cipher:
encode shift msg = map (chr . (+ shift) . ord) msg

decode shift msg = encode (negate shift) msg

-- Data.Map
-- association list, same as dict in python (k/v pair), order doesn't matter
--
-- simple example: find value of key in list of (k,v)
-- findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
-- findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing
-- TODO This won't terminate immedaitely after it finds k, right?  So would the
-- recursive solution be faster?
-- This is the "lookup" function from Data.List
--
-- fromList takes a list of (k,v) and returns a Data.Map.Map k v without
-- duplicate keys. keys must be Ord so haskell can arrange in a tree. Always
-- use Data.Map for k/v associations unless you have keys not part of Ord
-- typeclass.
--
-- check this out:
-- fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
-- fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty
--
-- lots of cool Map functions. one is fromListWith; takes a function to decide
-- what to do with duplicate keys instead of just throwing them away
-- neat example, dealing with multiple phone numbers:
-- phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
-- phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++
-- number2) xs
--
-- can do some nice things like passing max function here
--
-- similarly, there's Map.insertWith
-- Data.Set
-- internally implemented with trees, so they're ordered. ordering may not be
-- the same as the list when constructing a Set from fromList, though - it'll
-- be in type ordering (a,b,c; 1,2,3; etc.)
--
-- Set.fromList
--
-- intersection, difference, union, etc.
--
-- de-duplicating with sets much faster than nub, but xs have to be Ord. you'll
-- also lose list's original ordering with Set.
-- see Geometry.hs for example of creating your own module, pretty
-- straightforward
-- also striaghtforward for more complicated hierarhical namespacing - create
-- Geometry folder with files Sphere, Cube, Cuboid, call them Geometry.Sphere,
-- Geometry.Cube ... in headers
-- Making our own types and typeclasses
-- algebraic data types intro - how to make your own types?
-- like this!
-- data Bool = False | True
-- part after the = are "value constructors"
--
-- example type:
-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float
-- Circle and Rectangle above aren't types, they're value constructors - type
-- is Shape
--
-- Value constructions are actually functions that return a value of a data
-- type. In the above, the Floats signify parameters to the functions that
-- return Shapes.
--
-- In the below, you're pattern matching against constructors.
{- surface :: Shape -> Float -- can't do Circle -> Float, Circle not a type. You wouldn't do True -> Float if it was a Bool instead of a Shape.
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
-}
-- if you try to print some value of the Shape type above, you'll get an error
-- because the type isn't part of the Show typeclass. To make it part of that
-- typeclass, do:
-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
-- can make it a little more abstract, too:
data Point =
  Point Float
        Float
  deriving (Show)

data Shape
  = Circle Point
           Float
  | Rectangle Point
              Point
  deriving (Show)

-- then you have to modify surface:
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) =
  (abs $ x2 - x1) * (abs $ y2 - y1)

-- to export your data types, write type with value constructors in parens:
-- module Shapes
-- ( Point(..)
-- , Shape(..)
-- )
-- (..) exports all value constructors (Rectangle and Circle for Shape,)
-- can also just force people to make Shapes by using helper functions by not
-- exporting any value constructors. Map.Map does this.
--
-- Record syntax: alternative way to write data types with named components
{- data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show) -}
-- haskell automatically creates all those functions for you
-- now you can do this, and even put fields out of order:
-- Person {lastName="V", firstName="Mario", age=100, height=42.0, phoneNumber="123-4567", flavor="Arugula"}
--
-- Type parameters
-- Value constructor takes some parameters and produces a new value, as we see
-- above. Type constructors take types as parameters to produce new types.
-- Similar to templates in C++. Example:
-- data Maybe a = Nothing | Just a
-- ^ "a" is the type parameter. "Maybe" is a type constructor. You can have a
-- Maybe Int, but not a Maybe, because Maybe is not a type, just a type
-- constructor. Value "Just 'a'" has a type of "Maybe Char". List types are
-- done similarly.
-- Note that type of Nothing is Maybe a. Type is polymorphic. If a function
-- requires Maybe Char, you can give it a Nothing becaues it doesn't contain a
-- value so the concrete type doesn't really matter. Maybe a type can act like
-- a Maybe Int, just like 5 can act as an Int or Double. Type of empty list is
-- [a].
-- Another parameterized type: Map k v. If we're defining a mapping type, we
-- can add a typeclass constraint in the data declaration:
-- data (Ord k) => Map k v = ...
-- But there's a strong convention to never do this. Leads to more unnecessary
-- constraints in functions that use that type.
--
-- Derived instances
-- Typeclasses are more like interfaces in other languages than classes.
-- Below will automatically give you == and /= on a  Person type, assuming all
-- values in type can be equaled.
data Person = Person
  { firstName :: String
  , lastName :: String
  , age :: Int
  } deriving (Eq, Show, Read)

-- we can now use a Person in all functions that have the Eq class constraint
-- for the parameter
-- Read typeclass is basically inverse of Show. We can do:
mikeD =
  read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person

-- must use explicit type annotation since Haskell doesn't know what the result
-- of read will be before it executes
-- if you use the result of read later in a way that it has to be a Person,
-- Haskell will know that it should expect a Person
-- for deriving from Ord, if you have two values done with different
-- constructors, the first one in the type definition is considered smaller
-- data Bool = False | True deriving (Ord)
-- ^ in there, False < True
-- with Maybe a data type, Nothing is always less than Just, but if you have two
-- Justs, it goes inside the Just and compares the values directly according to
-- the type paramter.
-- bigger example:
data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Bounded types have lowest and highest possible value
-- Enum have predecessors and successors
-- Type synonyms
-- just different names
-- type String = [Char]
-- here's a useful example of using this to make things more readable
type PhoneNumber = String

type Name = String

type PhoneBook = [(Name, PhoneNumber)]

-- now you can use those in function definitions to give it a very pretty type
-- declaration
-- you can also parameterize type synonyms
type AssocList k v = [(k, v)]

-- you can also partially apply type parameters to get new type constructors
-- from them, just like you can partially apply functions
type IntMap v = Map.Map Int v -- now you can do this:

-- Map.fromList [(1, "hi"), (2, "bye")] :: IntMap String
-- another cool data type that takes two types as parameters: Either a b type
-- roughly:
-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
-- good to use instead of Maybe in some cases, because Maybe doesn't indicate
-- *how* something failed. a is a type that can tell us about possible failure
-- and b is type of succesful computation. so, errors use Left value constructor
-- and results use Right.
-- book has example with "locker lookup" functions
-- Recursive data structures
-- Constructor in an algebraic data type can have 0 or more fields and each
-- field most be a concrete type, as we've seen. You can also create *recursive*
-- data types. Think about [5]. It's really just 5:[]. So, a list can be an
-- empty list or an element joined with : and another list with 0 or more
-- elements.
-- So, you can implement a list like this:
-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
-- To more match the syntax of the actual list type, you can automatically
-- define a function as infix by making them comprised only of special
-- characters. You can do the same with constructors since they're functions
-- that return a data type. Turtles all the way down ...
infixr 5 :-:

data List a
  = Empty
  | a :-: (List a)
  deriving (Show, Read, Eq, Ord)

infixr 5 .++

(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

-- Notice the "fixity declaration". If you want, you can give operator functions
-- a fixity which states how tightly the operator binds and whether it's left or
-- right associative. Higher number means tighter binding (same as higher
-- precedence?). Pattern matching is actually about matching constructors. 8 or
-- 'a' are basically constructors for numeric and character types.
-- Book goes over implementation of binary search tree
-- Typeclasses 102
-- This is how Eq class defined in standard Prelude:
{- class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
-}
-- a is the type variable, will be instance of Eq
-- Didn't have to implement function bodies (or implement recursively), but we
-- will see soon how that helps
-- Here's how you make a type an instance of Eq by hand instead of deriving if
-- you want:
data TrafficLight
  = Red
  | Yellow
  | Green

instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

-- Recursive definition helps, since now we don't have to define /=
-- Let's make it an instance of Show, too
instance Show TrafficLight where
  show Red = "Stop!"
  show Yellow = "Speed up!"
  show Green = "Go go go!"
-- You can also make typeclasses subclasses of other typeclasses. Here's start
-- of Num:
-- class (Eq a) => Num a where
--    ...
-- Essentially says that you must make a type an instance of Eq before you can
-- make it an instance of Num
--
-- But, how are Maybe and other list types made instances of typeclasses?
-- They're type constructors, not concrete types ...
-- In definition of Eq, "a" is used as a concrete type because all types in
-- functions have to be concrete
-- Instead of just doing s/a/Maybe/, which is not allowed because Maybe isn't
-- concrete, you can do this:
{-
instance Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False
-}
-- This says that we make all types of the form Maybe something an instance of
-- Eq.
-- However, this is incorrect. We have no assurance that what the Maybe contains
-- can be used with Eq. So, I could be calling == on something that can't be
-- equated.
-- Instead, we must do something like this, adding a class constraint:
{-
instance (Eq m) => Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False
-}
-- Now, we're sure that x and y of type m can be equated.
-- In general, you must make sure that things in functions are of concrete types
-- by making the modifications seen above.
-- :info YourTypeClass in GHCI will show you what the instances of a typeclass
-- are. Works for types and type constructors, too, or type declaration of a
-- function.
-- A Yes-No typeclass
