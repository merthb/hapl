module Bead where

type Name = String

type Health = Integer

type Spell = (Integer -> Integer)

type Army = [Unit]

type EnemyArmy = Army

type Amount = Integer

showState :: (Show a) => State a -> String
showState = show

showMage :: Mage -> String
showMage = show

eqMage :: Mage -> Mage -> Bool
eqMage a b = a == b

showUnit :: Unit -> String
showUnit = show

-- showOneVOne = show

data State a = Alive a | Dead

instance (Show a) => Show (State a) where
  show Dead = "Dead"
  show (Alive a) = show a

instance (Eq a) => Eq (State a) where
  Alive a == Alive b = a == b
  Dead == Dead = True
  _ == _ = False

data Entity = Golem Health | HaskellElemental Health
  deriving (Show, Eq)

data Mage = Master Name Health Spell

instance Show Mage where
  show (Master name health _)
    | health < 5 = "Wounded " ++ name
    | otherwise = name

instance Eq Mage where
  (Master name1 health1 spell1) == (Master name2 health2 spell2) =
    name1 == name2 && health1 == health2 -- && spell1 1 == spell2 1

papi :: Mage
papi =
  let tunderpor enemyHP
        | enemyHP < 8 = 0
        | even enemyHP = div (enemyHP * 3) 4
        | otherwise = enemyHP - 3
   in Master "Papi" 126 tunderpor

java :: Mage
java = Master "Java" 100 (\x -> x - mod x 9)

traktor :: Mage
traktor = Master "Traktor" 20 (\x -> div (x + 10) (mod x 4 + 1))

jani :: Mage
jani = Master "Jani" 100 (\x -> x - div x 4)

skver :: Mage
skver = Master "Skver" 100 (\x -> div (x + 4) 2)

potionMaster :: Mage
potionMaster =
  let plx x
        | x > 85 = x - plx (div x 2)
        | x == 60 = 31
        | x >= 51 = 1 + mod x 30
        | otherwise = x - 7
   in Master "PotionMaster" 170 plx

data Unit = M (State Mage) | E (State Entity)

instance Show Unit where
  show (M (Alive a)) = show a
  show (E (Alive a)) = show a
  show _ = "Dead"

instance Eq Unit where
  (M a) == (M b) = a == b
  (E a) == (E b) = a == b
  _ == _ = False

formationFix :: Army -> Army
formationFix xs = filter f' xs ++ filter (not . f') xs
  where
    f (M Dead) = False
    f (E Dead) = False
    f _ = True

over :: Army -> Bool
over [] = True
over ((M Dead) : xs) = over xs
over ((E Dead) : xs) = over xs
over (_ : xs) = False

damage :: Unit -> Integer -> Unit
damage a x
  | x < 1 = a
  | otherwise = setHealth a (getHealth a - x)

heal :: Unit -> Integer -> Unit
heal a x
  | x < 1 = a
  | otherwise = setHealth a (getHealth a + x)

setHealth :: Unit -> Health -> Unit
setHealth (E (Alive (Golem x))) y = dead $ E (Alive (Golem y))
setHealth (E (Alive (HaskellElemental x))) y = dead $ E (Alive (HaskellElemental y))
setHealth (M (Alive (Master a x b))) y = dead $ M (Alive (Master a y b))
setHealth a _ = a

getHealth :: Unit -> Health
getHealth (E (Alive (Golem x))) = x
getHealth (E (Alive (HaskellElemental x))) = x
getHealth (M (Alive (Master _ x _))) = x
getHealth _ = 0

dead :: Unit -> Unit
dead a@(E (Alive (Golem x)))
  | x < 1 = E Dead
  | otherwise = a
dead a@(E (Alive (HaskellElemental x)))
  | x < 1 = E Dead
  | otherwise = a
dead a@(M (Alive (Master _ x _)))
  | x < 1 = M Dead
  | otherwise = a
dead a = a

applyDamage :: Unit -> Unit -> Unit
applyDamage (E (Alive (Golem _))) a = dead $ damage a 1
applyDamage (E (Alive (HaskellElemental _))) a = dead $ damage a 3
applyDamage _ a = a

fight :: EnemyArmy -> Army -> Army
fight [] [] = []
fight [] a = a
fight a [] = []
fight (M (Alive (Master _ _ f)) : xs) (y : ys) =
  setHealth y (f $ getHealth y) : fight xs (map (\x -> setHealth x (f $ getHealth x)) ys)
fight (x : xs) (y : ys) = applyDamage x y : fight xs ys

haskellDamage :: Army -> Integer
haskellDamage = foldl f 0
  where
    f acc x = acc + min 5 (getHealth x)

haskellBlast :: Army -> Army
haskellBlast xs = snd $ f xs 0
  where
    f xs n
      | null ys = (0, xs)
      | fst (f xs (n + 1)) > fst calc = f xs (n + 1)
      | otherwise = calc
      where
        ys = take 5 $ drop n xs
        calc = (haskellDamage ys, take n xs ++ map (\x -> damage x 5) ys ++ drop (n + 5) xs)

healable :: Unit -> Integer
healable a
  | getHealth a == 0 = 0
  | otherwise = 1

multiHeal :: Health -> Army -> Army
multiHeal y xs = f y xs 0
  where
    f _ [] _ = []
    f y xs n
      | over xs = xs
      | take n xs == xs = f y xs 0
      | y > 0 = f (y - healable (xs !! n)) (take n xs ++ [heal (xs !! n) 1] ++ drop (n + 1) xs) (n + 1)
      | otherwise = xs

chain :: Amount -> (Army, EnemyArmy) -> (Army, EnemyArmy)
chain _ ([], []) = ([], [])
chain n (x : xs, []) = (heal x n : xs, [])
chain n ([], ys) = ([], ys)
chain n (x : xs, y : ys)
  | n < 1 = (x : xs, y : ys)
  | otherwise = (heal x (n + d x) : fst (next ((n - 2) + deathCorrection)), damage y ((n - 1) + deathCorrection) : snd (next ((n - 2) + deathCorrection)))
  where
    next a = chain a (xs, ys)

    d (E Dead) = 1
    d (M Dead) = 1
    d _ = 0

    deathCorrection = d x + d y

data OneVOne = Winner String | You Health OneVOne | HaskellMage Health OneVOne deriving (Eq)

instance Show OneVOne where
  show a = "<" ++ show' a
    where
      show' (Winner s) = "|| Winner " ++ s ++ " ||>"
      show' (You hp x) = "You " ++ show hp ++ "; " ++ show' x
      show' (HaskellMage hp x) = "HaskellMage " ++ show hp ++ "; " ++ show' x

showOneVOne :: OneVOne -> String
showOneVOne = show