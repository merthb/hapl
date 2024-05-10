module Beadando where

showState a = show a
showMage a = show a
eqMage a b =  a == b
showUnit a = show a
showOneVOne a = show a 

type Name = String
type Health = Integer
type Spell = (Integer -> Integer)
type Army = [Unit]
type EnemyArmy = Army
type Amount = Integer

data State a = Alive a | Dead
    deriving(Eq)

instance Show a => Show (State a) where
    show (Alive a) = show a
    show Dead = "Dead"

data Entity = Golem Health  | HaskellElemental Health 
    deriving(Eq, Show)

getHealthEntity :: Entity -> Health
getHealthEntity (Golem a) = a
getHealthEntity (HaskellElemental a) = a

setHealthEntity :: Entity -> Health -> Entity
setHealthEntity (Golem a) n = Golem n
setHealthEntity (HaskellElemental a) n = HaskellElemental n

data Mage = Master Name Health Spell

getHealthMage :: Mage -> Health
getHealthMage (Master _ hp _) = hp

setHealthMage :: Mage -> Health -> Mage
setHealthMage (Master a hp b) n = (Master a n b)

papi = let 
    tunderpor enemyHP
        | enemyHP < 8 = 0
        | even enemyHP = div (enemyHP * 3) 4
        | otherwise = enemyHP - 3
    in Master "Papi" 126 tunderpor
java = Master "Java" 100 (\x ->  x - (mod x 9))
traktor = Master "Traktor" 20 (\x -> div (x + 10) ((mod x 4) + 1))
jani = Master "Jani" 100 (\x -> x - div x 4)
skver = Master "Skver" 100 (\x -> div (x+4) 2)

instance Show Mage where
    show (Master nev hp spell)
        | hp < 5 =  "Wounded " ++ nev
        | otherwise = nev

instance Eq Mage where
        (==) (Master nev1 hp1 spell1) (Master nev2 hp2 spell2) = nev1 == nev2 && hp1 == hp2

data Unit = M (State Mage) | E (State Entity)
    deriving(Eq)

instance Show Unit where
  show (M state) = mageState state
  show (E state) = entState state

mageState :: State Mage -> String
mageState (Alive mage) = showMage mage
mageState Dead = "Dead"

entState :: State Entity -> String
entState (Alive entity) = show entity
entState Dead = "Dead"

formationFix :: Army -> Army
formationFix [] = []
formationFix (x:xs) = formationHelper (x:xs) [] [] where
    formationHelper :: Army -> Army -> Army -> Army
    formationHelper [] a b = a ++ b
    formationHelper (x:xs) el halott
        | showUnit x == "Dead" = formationHelper xs el (halott++[x])
        | otherwise = formationHelper xs (el++[x]) halott

over :: Army -> Bool
over [] = True
over (x:xs)
    | showUnit x == "Dead" && over xs = True
    | otherwise = False

potionMaster = 
  let plx x
        | x > 85  = x - plx (div x 2)
        | x == 60 = 31
        | x >= 51 = 1 + mod x 30
        | otherwise = x - 7 
  in Master "PotionMaster" 170 plx

fight :: EnemyArmy -> Army -> Army
fight [] a = a
fight  a [] = []
fight ((E (Alive (Golem _))):xs) (y:ys)
    | isDead y = [y] ++ fight xs ys
    | ((getUnitHealth y)-1) <= 0 = [die(y)] ++ fight xs ys
    | otherwise = [setUnitHealth y ((getUnitHealth y)-1)] ++ fight xs ys
fight ((E (Alive (HaskellElemental _))):xs) (y:ys)
    | isDead y = [y] ++ fight xs ys
    | ((getUnitHealth y)-3) <= 0 = [die(y)] ++ fight xs ys
    | otherwise = [setUnitHealth y ((getUnitHealth y)-3)] ++ fight xs ys
fight ((M (Alive a)):xs) (y:ys) = mageFightHelp a [y] ++ fight xs (mageFightHelp a ys)
fight (_:xs) (y:ys) = [y] ++ fight xs (ys)

mageFightHelp :: Mage -> Army -> Army
mageFightHelp _ [] = []
mageFightHelp mage@(Master nev hp spell) (x:xs)
    | spell (getUnitHealth x) > 0 = [setUnitHealth x (spell (getUnitHealth x))] ++ mageFightHelp mage xs
    | otherwise = [die(x)] ++ mageFightHelp mage xs

haskellBlast :: Army -> Army
haskellBlast [] = []
haskellBlast [x]
    | (getUnitHealth x - 5) > 0 = [setUnitHealth x (getUnitHealth x - 5)]
    | otherwise = [die x]
haskellBlast (x:xs)
    | listcount (x:xs) <= 5 = [minusfive x] ++ haskellBlast xs
    | otherwise = reverse (drop (listcount (minusfivetimesfive (blastHelper (x:xs) []) 0)) (reverse (x:xs))) ++ minusfivetimesfive (blastHelper (x:xs) []) 0 where
        blastHelper :: Army -> Army -> Army
        blastHelper [] max = max 
        blastHelper (x:xs) max
            | sumoffivehp (x:xs) 0 > sumoffivehp max 0 = blastHelper xs (x:xs)
            | otherwise = blastHelper xs max

sumoffivehp :: Army -> Integer -> Health
sumoffivehp [] _ = 0
sumoffivehp (x:xs) 5 = 0
sumoffivehp (x:xs) n
    | isDead x = 0 + sumoffivehp xs (n+1)
    | getUnitHealth x >= 5  = 5 + sumoffivehp xs (n+1)
    | otherwise = (mod (getUnitHealth x) 5) + sumoffivehp xs (n+1)

nextFiveAllDead ::  Army -> Integer -> Bool
nextFiveAllDead [] _ = True
nextFiveAllDead _ 5 = True
nextFiveAllDead (x:xs) n = (getUnitHealth x) - 5 <= 0 && nextFiveAllDead xs (n+1)

minusfive :: Unit -> Unit
minusfive a
    | (getUnitHealth a)-5 > 0 = setUnitHealth a (getUnitHealth a - 5)
    | otherwise = die a

listcount :: Army -> Int
listcount [] = 0
listcount (x:xs) = 1 + listcount xs

getUnitHealth :: Unit -> Health
getUnitHealth (E (Alive a)) = getHealthEntity a
getUnitHealth (M (Alive a)) = getHealthMage a
getUnitHealth _ = 0

minusfivetimesfive :: Army -> Int -> Army
minusfivetimesfive [] _ = []
minusfivetimesfive a 5 = a
minusfivetimesfive (x:xs) n
    | isDead x = x : minusfivetimesfive (xs) (n+1)
    | otherwise = (minusfive x) : minusfivetimesfive (xs) (n+1)

multiHeal :: Health -> Army -> Army
multiHeal _ [] = []
multiHeal n [x]
    | n <= 0 = [x]
    | isDead x = [x]
    | otherwise = multiHeal (n-1) [healOne x]
multiHeal n (x:xs)
    | n <= 0 = (x:xs)
    | otherwise = healhelper n (x:xs) [] where
    healhelper :: Health -> Army -> Army -> Army
    healhelper 0 a maradek = maradek ++ a
    healhelper n [] maradek
        | notDeads maradek == [] = maradek
        | otherwise = healhelper n maradek []
    healhelper n (x:xs) maradek
        | isDead x = healhelper n xs (maradek ++ [x])
        | otherwise = healhelper (n-1) xs (maradek ++ [healOne x])

healOne :: Unit -> Unit
healOne x
    | isDead x = x
    | otherwise = setUnitHealth x ((getUnitHealth x) + 1)

setUnitHealth :: Unit -> Health -> Unit
setUnitHealth (E (Alive a)) n = E (Alive (setHealthEntity a n))
setUnitHealth (M (Alive a)) n = M (Alive (setHealthMage a n))
setUnitHealth a _ = a

isDead :: Unit -> Bool
isDead x
    | show x == "Dead" = True
    | otherwise = False

battle :: Army -> EnemyArmy -> Maybe Army
battle [] [] = Nothing
battle [] a = Just a
battle a [] = Just a
battle a b
    | notDeads a == [] && notDeads b == [] = Nothing
    | notDeads a == [] = Just b
    | notDeads b == [] = Just a
    | otherwise = battle (formationFix (whyMyTeam a b)) (formationFix (fight a b))

whyMyTeam :: Army -> EnemyArmy -> Army
whyMyTeam [] _ = []
whyMyTeam a b = multiHeal 20 (haskellBlast(fight b a))

notDeads :: Army -> Army
notDeads [] = []
notDeads x = filter (\x -> not (show x == "Dead")) x

chain :: Amount -> (Army, EnemyArmy) -> (Army, EnemyArmy)
chain _ ([],[]) = ([],[])
chain n (x,y)
    | n <= 0 = (x,y)
    | otherwise = chainHelper n (x,y) ([],[]) where
    chainHelper :: Amount -> (Army, EnemyArmy) -> (Army, EnemyArmy) -> (Army, EnemyArmy)
    chainHelper n ([],[]) (x,y) = (x,y)
    chainHelper n ([],b:bs) (x,y) = (x,y++b:bs)
    chainHelper n (a:as,[]) (x,y) = (x++[healN a n]++as, y)
    chainHelper 0 (a,b) (x,y) = (x++a, y++b)
    chainHelper n ((x:xs),(y:ys)) (a,b)
        | isDead x && isDead y = chainHelper n (xs,ys) (a ++ [x], b ++ [y])
        | isDead x = chainHelper (n-1) (xs,ys) (a ++ [x], b ++ [dmgN y n])
        | isDead y = chainHelper (n-1) (xs,ys) (a ++ [healN x n], b ++ [y])
        | n > 1 = chainHelper (n-2) (xs,ys) (a ++ [healN x n], b ++ [dmgN y (n-1)])
        | otherwise = chainHelper 0 (xs,ys) (a ++ [healN x n], b ++ [y])

healN :: Unit -> Health -> Unit
healN x n
    | isDead x = x
    | otherwise = setUnitHealth x ((getUnitHealth x) + n)

dmgN :: Unit -> Health -> Unit
dmgN x n
    | isDead x = x
    | (getUnitHealth x) - n <= 0 = die(x)
    | otherwise = setUnitHealth x ((getUnitHealth x) - n)

die :: Unit -> Unit
die (E (Alive a)) = E Dead
die (M (Alive a)) = M Dead
die a = a

battleWithChain :: Army -> EnemyArmy -> Maybe Army
battleWithChain [] [] = Nothing
battleWithChain [] a = Just a
battleWithChain a [] = Just a
battleWithChain a b
    | notDeads a == [] && notDeads b == [] = Nothing
    | notDeads a == [] = Just b
    | notDeads b == [] = Just a
    | otherwise = battleWithChain (formationFix(fst (chain 5 (whyMyTeam a b, (fight a b))))) (formationFix (snd (chain 5 (whyMyTeam a b, (fight a b)))))

data OneVOne = Winner String | You Health OneVOne | HaskellMage Health OneVOne deriving Eq

instance Show OneVOne where
    show (Winner a) = "<|| Winner " ++ a ++ " ||>"
    show (You n a) = "<You " ++ show n ++ "; " ++ maradek a ++ ">"
    show (HaskellMage n a) = "<HaskellMage " ++ show n ++ "; " ++ maradek a ++ ">"

maradek :: OneVOne -> String
maradek (Winner winner) = "|| Winner " ++ winner ++ " ||"
maradek (You n a) = "You " ++ show n ++ "; " ++ maradek a
maradek (HaskellMage n a) = "HaskellMage " ++ show n ++ "; " ++ maradek a