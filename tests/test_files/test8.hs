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

instance Show Mage where
    show (Master name hp _)
        | hp < 5 = "Wounded " ++ name
        | otherwise = name

instance Eq Mage where
    (==) (Master name1 hp1 _) (Master name2 hp2 _) = name1==name2 && hp1==hp2

data Unit = M (State Mage) | E (State Entity)
    deriving (Eq)

instance Show Unit where
    show (M a) = show a
    show (E a) = show a

formationFix :: Army -> Army
formationFix [] = []
formationFix army = filter (\ x -> x /= (M Dead) && x /= (E Dead)) army ++ filter (\ x -> x == (M Dead) || x == (E Dead)) army

over :: Army -> Bool
over [] = True
over (x:xs)
    | x == (E Dead) || x == (M Dead) = over xs
    | otherwise = False

potionMaster =
  let plx x
        | x > 85  = x - plx (div x 2)
        | x == 60 = 31
        | x >= 51 = 1 + mod x 30
        | otherwise = x - 7
  in Master "PotionMaster" 170 plx

checkDead :: Unit -> Unit
checkDead g@(E (Alive (Golem hp)))
    | hp <= 0 = (E Dead)
    | otherwise = g
checkDead h@(E (Alive (HaskellElemental hp)))
    | hp <= 0 = (E Dead)
    | otherwise = h
checkDead m@(M (Alive (Master _ hp _)))
    | hp <= 0 = (M Dead)
    | otherwise = m
checkDead d@(E Dead) = d
checkDead d@(M Dead) = d

damage :: Unit -> Spell -> Unit
damage d@(E Dead) _ = d
damage d@(M Dead) _ = d
damage (E (Alive (HaskellElemental n))) spell = checkDead (E (Alive (HaskellElemental (spell n))))
damage (E (Alive (Golem n))) spell = checkDead (E (Alive (Golem (spell n))))
damage (M (Alive (Master name hp spell1))) spell = checkDead (M (Alive (Master name (spell hp) spell1)))

getDamage :: Unit -> Integer
getDamage (E (Alive (Golem _))) = 1
getDamage (E (Alive (HaskellElemental _))) = 3
getDamage (E Dead) = 0
getDamage (M Dead) = 0

updateArmy :: Army -> Spell -> Army
updateArmy [] _ = []
updateArmy (x:xs) spell = damage x spell : updateArmy xs spell

fight :: EnemyArmy -> Army -> Army
fight [] army = army
fight _ [] = []
fight ((M (Alive (Master name hp spell))):xs) (y:ys) = damage y spell : fight xs (updateArmy ys spell)
fight (x:xs) (y:ys) = damage y (\old -> old - (getDamage x)) : fight xs ys

getHealth :: Unit -> Health
getHealth (E (Alive (Golem n))) = n
getHealth (E (Alive (HaskellElemental n))) = n
getHealth (M (Alive (Master _ hp _))) = hp
getHealth (M Dead) = 0
getHealth (E Dead) = 0

healthDiffs :: [Health] -> [Health] -> [Health]
healthDiffs x [] = x
healthDiffs [] x = x
healthDiffs (x:xs) (y:ys) = (x-y):(healthDiffs xs ys)

bombIt :: Army -> Integer -> Army
bombIt x 5 = x
bombIt [] _ = []
bombIt [x] _ = [damage x (\old -> old - 5)]
bombIt army@(x:xs) count = damage x (\old -> old - 5) : bombIt xs (count+1)

calcSumDamage :: Army -> Integer
calcSumDamage army = sum (healthDiffs (map getHealth army) (map getHealth (bombIt army 0)))

calcSums :: Army -> [Integer]
calcSums [] = []
calcSums (x:xs) = calcSumDamage (x:xs) : calcSums xs

haskellBlast :: Army -> Army
haskellBlast [] = []
haskellBlast army
    | over army = army
    | otherwise = blastHelp army [] where
    blastHelp :: Army -> Army -> Army
    blastHelp [] x = x
    blastHelp [x] [] = bombIt [x] 0
    blastHelp army@(x:xs) new
        | over army = army
        | calcSumDamage army == maximum (calcSums army) = new ++ bombIt army 0
        | otherwise = blastHelp xs (new++[x])

heal :: Unit -> Integer -> Unit
heal d@(E Dead) _ = d
heal d@(M Dead) _ = d
heal (E (Alive (HaskellElemental n))) amount = (E (Alive (HaskellElemental (n+amount))))
heal (E (Alive (Golem n))) amount = (E (Alive (Golem (n+amount))))
heal (M (Alive (Master name hp spell))) amount = (M (Alive (Master name (hp+amount) spell)))

isDead :: Unit -> Bool
isDead (M Dead) = True
isDead (E Dead) = True
isDead _ = False

multiHeal :: Health -> Army -> Army
multiHeal _ [] = []
multiHeal _ d@[(E Dead)] = d
multiHeal _ d@[(M Dead)] = d
multiHeal amount army
    | over army = army
    | amount < 0 = army
    | otherwise = multiHelp amount army [] where
    multiHelp :: Health -> Army -> Army -> Army
    multiHelp 0 army buffer = buffer ++ army
    multiHelp a [] buffer = multiHelp a buffer []
    multiHelp a army@(x:xs) buffer
        | isDead x = multiHelp a xs (buffer ++ [x])
        | otherwise = multiHelp (a-1) xs (buffer ++ [heal x 1])

battle :: Army -> EnemyArmy -> Maybe Army
battle [] [] = Nothing
battle x [] = Just x
battle [] x = Just x
battle x y
    | over x && over y = Nothing
    | over x = Just y
    | over y = Just x
    | otherwise = battle (formationFix $ multiHeal 20 $ haskellBlast $ fight y x) (formationFix $ fight x y)

combineTupleOfLists :: (Army, EnemyArmy) -> (Army, EnemyArmy) -> (Army, EnemyArmy)
combineTupleOfLists (fst1,snd1) (fst2,snd2) = (fst1++fst2,snd1++snd2)

chain :: Amount -> (Army, EnemyArmy) -> (Army, EnemyArmy)
chain 0 leftover = leftover
chain _ ([],[]) = ([],[])
chain amount start
    | amount < 0 = start
    | otherwise = chainHelp amount 0 start where
    chainHelp :: Amount -> Int -> (Army, EnemyArmy) -> (Army, EnemyArmy)
    chainHelp _ _ ([],[]) = ([],[])
    chainHelp 0 _ leftover = leftover
    chainHelp amount counter r@([],(y:ys))
        | mod counter 2 /= 0 = ([],(damage y (\hp -> hp-amount)):ys) 
        | otherwise = r
    chainHelp amount counter r@(x:xs,[])
        | mod counter 2 == 0 = ((heal x amount):xs,[]) 
        | otherwise = r
    chainHelp amount counter (x:xs,y:ys)
        | mod counter 2 == 0 && isDead x = combineTupleOfLists ([x],[]) (chainHelp (amount) (counter+1) (xs,y:ys))
        | mod counter 2 == 0 = combineTupleOfLists ([heal x amount],[]) (chainHelp (amount-1) (counter+1) (xs,y:ys))
        | mod counter 2 /= 0 && isDead y = combineTupleOfLists ([],[y]) (chainHelp (amount) (counter+1) (x:xs,ys))
        | mod counter 2 /= 0 = combineTupleOfLists ([],[damage y (\hp -> hp-amount)]) (chainHelp (amount-1) (counter+1) (x:xs,ys))

data OneVOne = Winner String | You Health OneVOne | HaskellMage Health OneVOne
    deriving Eq

instance Show OneVOne where
    show showdown = "<" ++ showOne showdown ++ ">" where
        showOne :: OneVOne -> String
        showOne (Winner a) = "|| Winner " ++ a ++ " ||"
        showOne (You hp xs) = "You " ++ show hp ++ "; " ++ showOne xs
        showOne (HaskellMage hp xs) = "HaskellMage " ++ show hp ++ "; " ++ showOne xs