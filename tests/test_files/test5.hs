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
    deriving (Eq)

instance Show a => Show (State a) where
    show (Alive x) = show x
    show Dead = "Dead"

data Entity = Golem Health | HaskellElemental Health
    deriving (Show, Eq)

data Mage = Master Name Health Spell

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
    show (Master name hp spell)
        | hp < 5 = "Wounded " ++  name
        | otherwise = name

instance Eq Mage where
    (==) (Master name1 hp1 spell1) (Master name2 hp2 spell2) = name1 == name2 && hp1 == hp2

data Unit = E (State Entity) | M (State Mage)
    deriving Eq

instance Show Unit where
    show (E x) = show x
    show (M x) = show x

formationFix :: Army -> Army
formationFix xs = aliveTroops xs ++ deadTroops xs where
    aliveTroops :: Army -> Army
    aliveTroops [] = []
    aliveTroops ((E (Alive x)):xs) = E (Alive x) : aliveTroops xs
    aliveTroops ((M (Alive x)):xs) = M (Alive x) : aliveTroops xs
    aliveTroops (x:xs) = aliveTroops xs
    deadTroops :: Army -> Army
    deadTroops [] = []
    deadTroops ((E Dead):xs) = E Dead : deadTroops xs
    deadTroops ((M Dead):xs) = M Dead : deadTroops xs
    deadTroops (x:xs) = deadTroops xs

over :: Army -> Bool
over [] = True
over ((E Dead):xs) = over xs
over ((M Dead):xs) = over xs
over _ = False

potionMaster = 
  let plx x
        | x > 85  = x - plx (div x 2)
        | x == 60 = 31
        | x >= 51 = 1 + mod x 30
        | otherwise = x - 7 
  in Master "PotionMaster" 170 plx

getHealth :: Unit -> Health
getHealth (E (Alive (Golem x))) = x
getHealth (E (Alive (HaskellElemental x))) = x
getHealth (M (Alive (Master name health spell))) = health
getHealth (E Dead) = 0
getHealth (M Dead) = 0

setHealth :: Unit -> Health -> Unit
setHealth (E (Alive (Golem x))) y
    | x + y > 0 = E (Alive (Golem (x + y)))
    | otherwise = E Dead
setHealth (E (Alive (HaskellElemental x))) y
    | x + y > 0 = E (Alive (HaskellElemental (x + y)))
    | otherwise = E Dead
setHealth (M (Alive (Master name health spell))) y
    | health + y > 0 = M (Alive (Master name (health + y) spell))
    | otherwise = M Dead
setHealth (E Dead) _ = E Dead
setHealth (M Dead) _ = M Dead

die :: Unit -> Unit
die (E (Alive (Golem x))) = E Dead
die (E (Alive (HaskellElemental x))) = E Dead
die (M (Alive (Master name health spell))) = M Dead
die (E Dead) = E Dead
die (M Dead) = M Dead

fight :: EnemyArmy -> Army -> Army
fight [] xs = xs
fight xs [] = []
fight ((E (Alive (Golem x))):xs) (y:ys)
    | getHealth y <= 1 = die y : fight xs ys
    | otherwise = setHealth y (-1) : fight xs ys
fight ((E (Alive (HaskellElemental x))):xs) (y:ys)
    | getHealth y <= 3 = die y : fight xs ys
    | otherwise = setHealth y (-3) : fight xs ys
fight ((M (Alive (Master name health spell))):xs) (y:ys) = head(castSpell (Master name health spell) (y:ys)) : fight xs (tail (castSpell (Master name health spell) (y:ys)))
fight (y:z:ys) ((E Dead):a:xs) = fight (z:ys) (a:xs)
fight (y:z:ys) ((M Dead):a:xs) = fight (z:ys) (a:xs)
fight (y:ys) (x:xs) = x :fight ys xs

castSpell :: Mage -> Army -> Army
castSpell _ [] = []
castSpell (Master name health spell) (E (Alive (Golem y)):xs)
    | getHealth (E (Alive (Golem (spell y)))) <= 0 = E Dead : castSpell (Master name health spell) xs
    | otherwise = E (Alive (Golem (spell y))) : castSpell (Master name health spell) xs
castSpell (Master name health spell) (E (Alive (HaskellElemental y)):xs)
    | getHealth (E (Alive (HaskellElemental (spell y)))) <= 0 = E Dead : castSpell (Master name health spell) xs
    | otherwise = E (Alive (HaskellElemental (spell y))) : castSpell (Master name health spell) xs
castSpell (Master name health spell) (M (Alive (Master enemyname enemyhealth enenmyspell)):xs)
    | getHealth (M (Alive (Master enemyname (spell enemyhealth) enenmyspell))) <= 0 = M Dead : castSpell (Master name health spell) xs
    | otherwise = M (Alive (Master enemyname (spell enemyhealth) enenmyspell)) : castSpell (Master name health spell) xs
castSpell (Master name health spell) (x:xs) = x : castSpell (Master name health spell) xs

haskellBlast :: Army -> Army
haskellBlast [] = []
haskellBlast [a] = [setHealth a (-5)]
haskellBlast [a, b] = [setHealth a (-5), setHealth b (-5)]
haskellBlast [a, b, c] = [setHealth a (-5), setHealth b (-5), setHealth c (-5)]
haskellBlast [a, b, c, d] = [setHealth a (-5), setHealth b (-5), setHealth c (-5), setHealth d (-5)]
haskellBlast (x:xs)
    | sum (map getHealth (take 5 modifiedarray)) == maximumdamage = haskellBlastHelper (take 5 (x:xs)) ++ drop 5 (x:xs)
    | otherwise = x : haskellBlast xs  where
        modifiedarray = modifyHealths (x:xs)
        maximumdamage = maximumdmg $ map getHealth modifiedarray
        haskellBlastHelper :: Army -> Army
        haskellBlastHelper [] = []
        haskellBlastHelper (x:xs) = setHealth x (-5) : haskellBlastHelper xs

modifyHealths :: Army -> Army
modifyHealths [] = []
modifyHealths (x:xs)
    | getHealth x <= 5 = x : modifyHealths xs
    | otherwise = setHealth x (-1 * getHealth x + 5) :  modifyHealths xs

maximumdmg :: [Integer] -> Integer
maximumdmg [] = 0
maximumdmg xs = maximum [sum (take 5 (drop i xs)) | i <- [0..length xs - 5]]

multiHeal :: Health -> Army -> Army
multiHeal 0 xs = xs
multiHeal n [] = []
multiHeal n [E Dead] = [E Dead]
multiHeal n [M Dead] = [M Dead]
multiHeal n (x:xs) 
    | n < 0 = (x:xs)
    | otherwise = multiHealHelper n (x:xs) [] where
    multiHealHelper :: Health -> Army -> Army -> Army
    multiHealHelper 0 xs ys = ys ++ xs
    multiHealHelper n [] ys = multiHealHelper n ys []
    multiHealHelper n ((E Dead):xs) ys = multiHealHelper n xs (ys ++ [E Dead]) 
    multiHealHelper n ((M Dead):xs) ys = multiHealHelper n xs (ys ++ [M Dead])
    multiHealHelper n (x:xs) ys = multiHealHelper (n - 1) xs (ys ++ [setHealth x 1])

data OneVOne = Winner String | You Health OneVOne | HaskellMage Health OneVOne 
    deriving Eq

instance Show OneVOne where
    show (Winner x) = "<" ++ format ("|| Winner " ++ x ++ " ||>")
    show (You x y) = "<" ++ format("You " ++ show x ++ "; " ++ show y)
    show (HaskellMage x y) = "<" ++ format("HaskellMage " ++ show x ++ "; " ++ show y)

format :: String -> String
format [] = []
format (x:xs)
    | x == '<' = format xs
    | otherwise = x: format xs

-- finalBattle :: Health -> Health -> OneVOne
-- finalBattle x y
--     | x > 0 && y <= 0 = Winner "You"
--     | x <= 0 && y > 0 = Winner "HaskellMage"
--     | x < 4 && y > 0 = 