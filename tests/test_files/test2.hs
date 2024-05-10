module Nagybeadando where
import Data.List
import Data.Maybe

showState a = show a
showMage a = show a
eqMage a b = a == b
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
potionMaster = 
  let plx x
        | x > 85  = x - plx (div x 2)
        | x == 60 = 31
        | x >= 51 = 1 + mod x 30
        | otherwise = x - 7 
  in Master "PotionMaster" 170 plx

instance Show Mage where
    show (Master name health spell) 
        | 5 > health = "Wounded " ++ name
        | otherwise = name

instance Eq Mage where
    (==) (Master name1 health1 spell1) (Master name2 health2 spell2) =
        name1 == name2 && health1 == health2

data Unit = M (State Mage) | E (State Entity)
    deriving (Eq)

instance Show Unit where
    show (M (Alive mage)) = show mage
    show (E (Alive entity)) = show entity
    show (M Dead) = "Dead"
    show (E Dead) = "Dead"

formationFix :: Army -> Army
formationFix army = filter isAlive army ++ filter isDead army

isAlive :: Unit -> Bool
isAlive (M (Alive _)) = True
isAlive (E (Alive _)) = True
isAlive _ = False

isDead :: Unit -> Bool
isDead (M Dead) = True
isDead (E Dead) = True
isDead _ = False

over :: Army -> Bool
over [] = True
over [E (Dead)] = True
over [M (Dead)] = True
over (x:xs) 
    | isAlive x == True = False
    | otherwise = over xs

hpCheck :: Unit -> Unit
hpCheck (M (Alive mage@(Master name health spell)))
    | health <= 0 = M Dead
    | otherwise = M (Alive mage)
hpCheck (E (Alive entity@(Golem health)))
    | health <= 0 = E Dead
    | otherwise = E (Alive entity)
hpCheck (E (Alive entity@(HaskellElemental health)))
    | health <= 0 = E Dead
    | otherwise = E (Alive entity)
hpCheck (M Dead) = M Dead
hpCheck (E Dead) = E Dead

damageCal :: Unit -> Integer -> Unit
damageCal (M (Alive mage@(Master name health spell))) dmg
    | health - dmg <= 0 = M Dead
    | otherwise = M (Alive (Master name (health - dmg) spell))
damageCal (E (Alive entity@(Golem health))) dmg
    | health - dmg <= 0 = E Dead
    | otherwise = E (Alive (Golem (health - dmg)))
damageCal (E (Alive entity@(HaskellElemental health))) dmg
    | health - dmg <= 0 = E Dead
    | otherwise = E (Alive (HaskellElemental (health - dmg)))
damageCal (M Dead) _ = M Dead
damageCal (E Dead) _ = E Dead

getHealth :: Unit -> Health
getHealth (M (Alive (Master _ health _))) = health
getHealth (E (Alive (Golem health))) = health
getHealth (E (Alive (HaskellElemental health))) = health
getHealth (M Dead) = 0 
getHealth (E Dead) = 0 

fight :: EnemyArmy -> Army -> Army
fight [] ys = ys
fight xs [] = []
fight (x:xs) (y:ys) =
    case x of
        (E (Dead)) -> y : fight xs ys
        (M (Dead)) -> y : fight xs ys
        (E (Alive (Golem hp))) ->
            if isAlive y
                then damageCal y 1 : fight xs ys
                else y : fight xs ys
        (E (Alive (HaskellElemental hp))) ->
            if isAlive y
                then damageCal y 3 : fight xs ys
                else y : fight xs ys
        (M (Alive (Master n1 hp1 spell))) ->
            case y of
                (E (Alive (Golem hp))) ->
                    hpCheck (E (Alive (Golem (spell hp)))) : fight xs (spellDamage ys spell)
                (E (Alive (HaskellElemental hp))) ->
                    hpCheck (E (Alive (HaskellElemental (spell hp)))) : fight xs (spellDamage ys spell)
                (M (Alive (Master n2 hp2 enemySpell))) ->
                    hpCheck (M (Alive (Master n2 (spell hp2) enemySpell)))  : fight xs (spellDamage ys spell)
                (E (Dead)) -> y : fight xs (spellDamage ys spell)
                (M (Dead)) -> y : fight xs (spellDamage ys spell)

spellDamage :: Army -> Spell -> Army
spellDamage [] spell = []
spellDamage (y:ys) spell = case y of
                (E (Alive (Golem enemyHP))) ->
                    hpCheck (E (Alive (Golem (spell enemyHP)))) : spellDamage ys spell
                (E (Alive (HaskellElemental enemyHP))) ->
                    hpCheck (E (Alive (HaskellElemental (spell enemyHP)))) : spellDamage ys spell
                (M (Alive (Master name enemyHP spell1))) -> 
                    hpCheck (M (Alive (Master name (spell enemyHP) spell1))) : spellDamage ys spell
                (E (Dead)) -> 
                    y : spellDamage ys spell
                (M (Dead)) -> 
                    y : spellDamage ys spell

haskellBlast :: Army -> Army
haskellBlast [] = []
haskellBlast army@(x:xs)
    | all isDead (x:xs) = (x:xs)
    | isAlive(x) && all isDead xs = damageCal x 5 : xs
    | otherwise = blastHelper (bgroups(army)) (sorszam army)

bgroups :: Army -> [Int]
bgroups [] = []
bgroups army = blastGroup (take 5 army) 0 : bgroups (drop 1 army)

blastGroup :: Army -> Int -> Int
blastGroup [] acc = acc
blastGroup (unit:rest) acc
    | getHealth unit - 5 >= 0 = blastGroup rest (acc + 5)
    | otherwise = blastGroup rest (acc + fromInteger (getHealth unit))

blastHelper :: [Int] -> [(Unit, Int)] -> Army
blastHelper ys [] = []
blastHelper ys army@((unit, int):xs) 
    | elemIndex (maximum ys) ys == Just int = applyDamage (take 5 (map fst army)) ++ drop 4 (map fst xs)
    | otherwise = unit : blastHelper ys xs

applyDamage :: [Unit] -> [Unit]
applyDamage [] = []
applyDamage (x:xs) = hpCheck(damageCal x 5) : applyDamage xs

sorszam :: Army -> [(Unit, Int)]
sorszam army = zip army [0..] 

multiHeal :: Health -> Army -> Army
multiHeal _ [] = []
multiHeal _ [E Dead] = [E Dead]
multiHeal healAmount units 
    | longerThan (fromIntegral healAmount) (filter isAlive units) = healHelper healAmount units (fromIntegral healAmount)
    | otherwise = healHelper healAmount units (length (filter isAlive units))

healHelper :: Health -> Army -> Int -> Army
healHelper _ [] _ = []
healHelper hp (x:xs) n
    | hp <= 0 = x:xs
    | isAlive x = heal x healedAmount : healHelper remainingHealth xs (n - 1)
    | otherwise = x : healHelper hp xs n
    where
        healedAmount = hp `div` fromIntegral n + if hp `mod` fromIntegral n > 0 && n > 0 then 1 else 0
        remainingHealth = hp - healedAmount

heal :: Unit -> Health -> Unit
heal (M (Alive (Master name hp spell))) n = M (Alive (Master name (hp + n) spell))
heal (E (Alive (Golem hp))) n = E (Alive (Golem (hp + n)))
heal (E (Alive (HaskellElemental hp))) n = E (Alive (HaskellElemental (hp + n)))

isNonEmpty :: [a] -> Bool
isNonEmpty [] = False
isNonEmpty (_:_) = True

longerThan :: Int -> [a] -> Bool
longerThan n xs = isNonEmpty $ drop n xs
