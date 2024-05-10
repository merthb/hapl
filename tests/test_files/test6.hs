module Nagybeadando where

import Data.List
import Data.Maybe

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

data Entity = Golem Health | HaskellElemental Health
    deriving(Eq, Show)

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
potionMaster =
  let plx x
        | x > 85  = x - plx (div x 2)
        | x == 60 = 31
        | x >= 51 = 1 + mod x 30
        | otherwise = x - 7
  in Master "PotionMaster" 170 plx

instance Show Mage where
    show (Master nev hp spell)
        | hp < 5 = "Wounded" ++ " " ++ nev
        | otherwise = nev

instance Eq Mage where
    (Master nev1 hp1 spell1) == (Master nev2 hp2 spell2) = nev1 == nev2 && hp1 == hp2

data Unit = M (State Mage) | E (State Entity)
    deriving(Eq)

instance Show Unit where
    show (M (Alive mage)) = show mage
    show (E (Alive entity)) = show entity
    show (M Dead) = "Dead"
    show (E Dead) = "Dead"

formationFix :: Army -> Army
formationFix xs = filter isAlive xs ++ filter isDead xs where
    isAlive (E (Alive _)) = True
    isAlive (M (Alive _)) = True
    isAlive _ = False
    isDead (E Dead) = True
    isDead (M Dead) = True
    isDead _ = False

isDead :: Unit -> Bool
isDead (M (Alive mage)) = False
isDead (E (Alive entity)) = False
isDead (M dead) = True
isDead (E dead) = True

over :: Army -> Bool
over [] = True
over (x:xs)
    | not (isDead x) = False
    | otherwise = over xs

getState :: Unit -> Health
getState (M (Alive (Master name health spell))) = health
getState (E (Alive (Golem health))) = health
getState (E (Alive (HaskellElemental health))) = health
getState _ = 0

setState :: Unit -> Health -> Unit
setState (M (Alive (Master name health spell))) newHealth = M (Alive (Master name newHealth spell))
setState (E (Alive (Golem health))) newHealth = E (Alive (Golem newHealth))
setState (E (Alive (HaskellElemental health))) newHealth = E (Alive (HaskellElemental newHealth))
setState a _ = a

seged :: Army -> Spell -> Army
seged [] f = []
seged (x:xs) f
    | getState (setState x (f (getState x))) > 0 = setState x (f (getState x)) : seged xs f
    | otherwise = makeDead x : seged xs f

makeDead :: Unit -> Unit
makeDead (M (Alive (Master name health spell))) = M Dead
makeDead (E (Alive (Golem health))) = E Dead
makeDead (E (Alive (HaskellElemental health))) = E Dead
makeDead (M Dead) = M Dead
makeDead (E Dead) = E Dead

fight :: EnemyArmy -> Army -> Army
fight _ [] = []
fight [] ys = ys
fight ((E (Alive (Golem hp1))):xs) ((E (Alive (Golem hp2))):ys)
    | hp2 - 1 <= 0 = makeDead (E (Alive (Golem hp2))) : fight xs ys
    | otherwise = E (Alive (Golem (hp2 - 1))) : fight xs ys
fight ((E (Alive (Golem hp1))):xs) ((E (Alive (HaskellElemental hp2))):ys)
    | hp2 - 1 <= 0 = makeDead (E (Alive (HaskellElemental hp2))) : fight xs ys
    | otherwise = E (Alive (HaskellElemental (hp2 - 1))) : fight xs ys
fight ((E (Alive (Golem hp1))):xs) ((M (Alive (Master name hp2 spell))):ys)
    | hp2 - 1 <= 0 = makeDead (M (Alive (Master name hp2 spell))) : fight xs ys
    | otherwise = M (Alive (Master name (hp2-1) spell)) : fight xs ys
fight ((E (Alive (Golem hp1))):xs) (E Dead:ys) = E Dead : fight xs ys
fight ((E (Alive (Golem hp1))):xs) (M Dead:ys) = M Dead : fight xs ys
fight ((E (Alive (HaskellElemental hp1))):xs) ((E (Alive (Golem hp2))):ys)
    | hp2 - 3 <= 0 = makeDead (E (Alive (Golem hp2))) : fight xs ys
    | otherwise = E (Alive (Golem (hp2 - 3))) : fight xs ys
fight ((E (Alive (HaskellElemental hp1))):xs) ((E (Alive (HaskellElemental hp2))):ys)
    | hp2 - 3 <= 0 = makeDead (E (Alive (HaskellElemental hp2))) : fight xs ys
    | otherwise = E (Alive (HaskellElemental (hp2 - 3))) : fight xs ys
fight ((E (Alive (HaskellElemental hp1))):xs) ((M (Alive (Master name hp2 spell))):ys)
    | hp2 - 3 <= 0 = makeDead (M (Alive (Master name hp2 spell))) : fight xs ys
    | otherwise = M (Alive (Master name (hp2-3) spell)) : fight xs ys
fight ((E (Alive (HaskellElemental hp1))):xs) (E Dead:ys) = E Dead : fight xs ys
fight ((E (Alive (HaskellElemental hp1))):xs) (M Dead:ys) = M Dead : fight xs ys
fight ((M (Alive (Master name health spell))):xs) ((E (Alive (Golem hp2))):ys)
    | spell hp2 <= 0 = makeDead (E (Alive (Golem hp2))) : fight xs (seged ys spell)
    | otherwise = E (Alive (Golem (spell hp2))) : fight xs (seged ys spell)
fight ((M (Alive (Master name health spell))):xs) ((E (Alive (HaskellElemental hp2))):ys)
    | spell hp2 <= 0 = makeDead (E (Alive (HaskellElemental hp2))) : fight xs (seged ys spell)
    | otherwise = E (Alive (HaskellElemental (spell hp2))) : fight xs (seged ys spell)
fight ((M (Alive (Master name health spell))):xs) ((M (Alive (Master name2 hp2 spell2))):ys)
    | spell hp2 <= 0 = makeDead (M (Alive (Master name2 hp2 spell2))) : fight xs (seged ys spell)
    | otherwise = M (Alive (Master name2 (spell hp2) spell2)) : fight xs (seged ys spell)
fight ((M (Alive (Master name health spell))):xs) (E Dead:ys) = E Dead : fight xs (seged ys spell)
fight ((M (Alive (Master name health spell))):xs) (M Dead:ys) = M Dead : fight xs (seged ys spell)
fight (E Dead:xs) ((E (Alive (Golem hp2))):ys) = E (Alive (Golem hp2)) : fight xs ys
fight (M Dead:xs) ((E (Alive (Golem hp2))):ys) = E (Alive (Golem hp2)) : fight xs ys
fight (E Dead:xs) ((E (Alive (HaskellElemental hp2))):ys) = E (Alive (HaskellElemental hp2)) : fight xs ys
fight (M Dead:xs) ((E (Alive (HaskellElemental hp2))):ys) = E (Alive (HaskellElemental hp2)) : fight xs ys
fight (E Dead:xs) ((M (Alive (Master name health spell))):ys) = M (Alive (Master name health spell)) : fight xs ys
fight (M Dead:xs) ((M (Alive (Master name health spell))):ys) = M (Alive (Master name health spell)) : fight xs ys
fight (E Dead:xs) (E Dead:ys) = E Dead : fight xs ys
fight (E Dead:xs) (M Dead:ys) = M Dead : fight xs ys
fight (M Dead:xs) (E Dead:ys) = E Dead : fight xs ys
fight (M Dead:xs) (M Dead:ys) = M Dead : fight xs ys

bigBoiSum :: [Int] -> [Int]
bigBoiSum xs = countHelper xs where
    countHelper :: [Int] -> [Int]
    countHelper [] = []
    countHelper xs
        | length xs >= 5 = sum (take 5 (map (\x -> min x 5) xs)) : countHelper (drop 1 xs)
        | otherwise = []

haskellBlast :: Army -> Army
haskellBlast [] = []
haskellBlast xs = seged xs where
    seged :: Army -> Army
    seged [] = []
    seged xs
        | elemIndex (maximum counts) counts /= Nothing = blast  (fromJust (elemIndex (maximum counts) counts)) xs
        | otherwise = map (\x -> case x of E Dead -> E Dead; M Dead -> M Dead; _ -> decreaseHealth x 5) xs where
            counts = bigBoiSum (map (fromIntegral . getState) xs)

blast ::  Int -> Army -> Army
blast _ [] = []
blast maxIndex xs = map (\(i, x) -> if i >= maxIndex && i <= maxIndex + 4 then applyDamage 5 x else x) (zip [0..] xs)

applyDamage :: Int -> Unit -> Unit
applyDamage damage (E Dead) = E Dead
applyDamage damage (M Dead) = M Dead
applyDamage damage (E (Alive (Golem health)))
    | fromIntegral health - fromIntegral damage <= 0 = E Dead
    | otherwise = E (Alive (Golem (fromIntegral health - fromIntegral damage)))
applyDamage damage (E (Alive (HaskellElemental health)))
    | fromIntegral health - fromIntegral damage <= 0 = E Dead
    | otherwise = E (Alive (HaskellElemental (fromIntegral health - fromIntegral damage)))
applyDamage damage (M (Alive (Master name health spell)))
    | fromIntegral health - fromIntegral damage <= 0 = M Dead
    | otherwise = M (Alive (Master name (fromIntegral health - fromIntegral damage) spell))

decreaseHealth :: Unit -> Health -> Unit
decreaseHealth (E Dead) _ = E Dead
decreaseHealth (M Dead) _ = M Dead
decreaseHealth (E (Alive (Golem health))) n
    | health - n <= 0 = E Dead
    | otherwise = E (Alive (Golem (health - n)))
decreaseHealth (E (Alive (HaskellElemental health))) n
    | health - n <= 0 = E Dead
    | otherwise = E (Alive (HaskellElemental (health - n)))
decreaseHealth (M (Alive (Master name health spell))) n
    | health - n <= 0 = M Dead
    | otherwise = M (Alive (Master name (health - n) spell))

increaseHealth :: Unit -> Health -> Unit
increaseHealth (E Dead) _ = E Dead
increaseHealth (M Dead) _ = M Dead
increaseHealth (E (Alive (Golem health))) n = E (Alive (Golem (health + n)))
increaseHealth (E (Alive (HaskellElemental health))) n = E (Alive (HaskellElemental (health + n)))
increaseHealth (M (Alive (Master name health spell))) n = M (Alive (Master name (health + n) spell))

armyLength :: Army -> Int
armyLength [] = 0
armyLength (x:xs)
    | (not . isDead) x = 1 + armyLength xs
    | otherwise = armyLength xs

multiHeal :: Health -> Army -> Army
multiHeal _ [] = []
multiHeal n xs 
    | n <= 0 = xs
multiHeal n [E Dead] = [E Dead]
multiHeal n [M Dead] = [M Dead]
multiHeal n [x] = [increaseHealth x n]
multiHeal n xs = seged n xs (div (fromIntegral n) (armyLength xs))      (mod (fromIntegral n) (armyLength xs)) where
    seged :: Health -> Army -> Int -> Int -> Army
    seged _ [] _ _ = []
    seged n army@(x:xs) osztas maradek
        | isDead x = x : seged n xs osztas maradek
        | not (rovidebb army n) && n > 0 = increaseHealth x 1 : seged (n - 1) xs 0 0
        | rovidebb army n && (not . isDead) x && osztas >  0 && maradek >  0 = increaseHealth x (fromIntegral osztas + 1) : seged n xs osztas (maradek-1)
        | rovidebb army n && (not . isDead) x && osztas == 0 && maradek >  0 = increaseHealth x 1 : seged n xs osztas (maradek-1)
        | rovidebb army n && (not . isDead) x && osztas >  0 && maradek == 0 = increaseHealth x (fromIntegral osztas) : seged n xs osztas maradek
        | otherwise = x : xs

rovidebb :: Army -> Integer -> Bool
rovidebb [] _ = True
rovidebb _ 0  = False
rovidebb (x:xs) szam
    | (not. isDead) x = rovidebb xs (szam-1)
    | otherwise = rovidebb xs szam

data OneVOne = Winner String | You Health OneVOne | HaskellMage Health OneVOne deriving Eq

instance Show OneVOne where
    show (Winner nev) = "<|| Winner " ++ nev ++ " ||>"
    show (You health satobbi) = "<You " ++ show health ++ "; " ++ drop 1 (show satobbi)
    show (HaskellMage health satobbi) = "<HaskellMage " ++ show health ++ "; " ++ drop 1 (show satobbi)

battle :: Army -> EnemyArmy -> Maybe Army
battle [] [] = Nothing
battle xs [] = Just xs
battle [] ys = Just ys
battle xs ys
    | over xs && over ys = Nothing
    | over xs = Just ys
    | over ys = Just xs
    | otherwise = battle (formationFix $ multiHeal 20 $ haskellBlast $ fight ys xs) (formationFix $ fight xs ys)
