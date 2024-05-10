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
    deriving Eq

instance Show a => Show (State a) where
    show (Alive x) = show x
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

instance Eq Mage where
    (Master name1 hp1 _) == (Master name2 hp2 _) = name1 == name2 && hp1 == hp2

instance Show Mage where
    show (Master a n _)
        | n < 5 = "Wounded " ++ a
        | otherwise = a

data Unit = M (State Mage) | E (State Entity)

instance Show Unit where
    show (M mageState) = case mageState of
                            Alive mage -> showMage mage
                            Dead -> "Dead"
    show (E entityState) = case entityState of
                            Alive entity -> show entity
                            Dead -> "Dead"

instance Eq Unit where
    (M (Alive (Master name1 _ _))) == (M (Alive (Master name2 _ _))) = name1 == name2
    (E (Alive (Golem hp1))) == (E (Alive (Golem hp2))) = hp1 == hp2
    (E (Alive (HaskellElemental hp1))) == (E (Alive (HaskellElemental hp2))) = hp1 == hp2
    (M Dead) == (M Dead) = True
    (E Dead) == (E Dead) = True
    _ == _ = False

formationFix :: Army -> Army
formationFix army = alive ++ dead where
    alive = filter isAlive army
    dead = filter (not . isAlive) army
    isAlive (M (Alive _)) = True
    isAlive (E (Alive _)) = True
    isAlive _ = False

over :: Army -> Bool
over [] = True
over (x:xs) = case x of
    M (Alive _) -> False
    E (Alive _) -> False
    _ -> over xs

potionMaster = 
  let plx x
        | x > 85  = x - plx (div x 2)
        | x == 60 = 31
        | x >= 51 = 1 + mod x 30
        | otherwise = x - 7
  in Master "PotionMaster" 170 plx

applyDamage :: Health -> Unit -> Unit
applyDamage dmg (E (Alive (Golem hp))) = if hp - dmg <= 0 then E Dead else E (Alive (Golem (hp - dmg)))
applyDamage dmg (E (Alive (HaskellElemental hp))) = if hp - dmg <= 0 then E Dead else E (Alive (HaskellElemental (hp - dmg)))
applyDamage dmg (M (Alive (Master name hp spell))) = if hp - dmg <= 0 then M Dead else M (Alive (Master name (hp - dmg) spell))
applyDamage _ unit = unit

getHealth :: Unit -> Health
getHealth (E (Alive (Golem hp))) = hp
getHealth (E (Alive (HaskellElemental hp))) = hp
getHealth (M (Alive (Master _ hp _))) = hp
getHealth _ = 0

applyMageAttacks :: Unit -> MageAttacks -> Unit
applyMageAttacks unit [] = unit
applyMageAttacks (E (Alive (Golem hp))) (sp:sps) = if (sp hp) <= 0 then E Dead else applyMageAttacks (E (Alive (Golem (sp hp)))) sps
applyMageAttacks (E (Alive (HaskellElemental hp))) (sp:sps) = if (sp hp) <= 0 then E Dead else applyMageAttacks (E (Alive (HaskellElemental (sp hp)))) sps
applyMageAttacks (M (Alive (Master name hp spell))) (sp:sps) = if (sp hp) <= 0 then M Dead else applyMageAttacks (M (Alive (Master name (sp hp) spell))) sps
applyMageAttacks unit _ = unit

type MageAttacks = [Spell]

fight :: EnemyArmy -> Army -> Army
fight [] ys = ys
fight xs [] = []
fight xs ys = helper xs ys [] where
    helper :: EnemyArmy -> Army -> MageAttacks -> Army
    helper [] ys [] = ys
    helper xs [] _ = []
    helper ((M Dead):xs) (y:ys) ms = (applyMageAttacks y ms) : helper xs ys ms
    helper ((E Dead):xs) (y:ys) ms = (applyMageAttacks y ms): helper xs ys ms
    helper ((E (Alive (Golem hp))):xs) (y:ys) ms = (applyDamage 1 (applyMageAttacks y ms)): helper xs ys ms
    helper ((E (Alive (HaskellElemental hp))):xs) (y:ys) ms = (applyDamage 3 (applyMageAttacks y ms)): helper xs ys ms
    helper ((M (Alive (Master _ hp sp))):xs) (y:ys) ms = (applyMageAttacks y (ms++[sp])) : helper xs ys (ms++[sp])
    helper [] (y:ys) ms = (applyMageAttacks y ms) : helper [] ys ms

haskellBlast :: Army -> Army
haskellBlast army
  | length army < 5 = map (applyDamage 5) army
  | otherwise = let
      bestStart = findBestStart 0 0 0
      (beforeBlast, rest) = splitAt bestStart army
      (blastArea, afterBlast) = splitAt 5 rest
    in beforeBlast ++ map (applyDamage 5) blastArea ++ afterBlast
  where
    findBestStart :: Int -> Int -> Int -> Int
    findBestStart idx bestDamage bestIdx
      | idx > length army - 5 = bestIdx
      | otherwise = 
          let currentDamage = sum . map potDam $ take 5 (drop idx army)
          in if currentDamage > bestDamage
             then findBestStart (idx + 1) currentDamage idx
             else findBestStart (idx + 1) bestDamage bestIdx

    potDam :: Unit -> Int
    potDam (E (Alive (Golem hp))) = min 5 (fromIntegral hp)
    potDam (E (Alive (HaskellElemental hp))) = min 5 (fromIntegral hp)
    potDam (M (Alive (Master _ hp _))) = min 5 (fromIntegral hp)
    potDam _ = 0

multiHeal :: Health -> Army -> Army
multiHeal 0 army = army
multiHeal _ [] = []
multiHeal totalHeal army
    | totalHeal < 0 = army
    | remainHeal army totalHeal == totalHeal = army
    | remainHeal army totalHeal == 0 = healOne army totalHeal
    | otherwise = multiHeal (remainHeal army totalHeal) (healOne army totalHeal)

remainHeal :: Army -> Health -> Health
remainHeal [] x = x
remainHeal (unit:units) healLeft
    | healLeft <= 0 = 0
    | isAlive unit = remainHeal units (healLeft - 1)
    | otherwise = remainHeal units healLeft

healOne :: Army -> Health -> Army
healOne [] _ = []
healOne (unit:units) healLeft
    | healLeft <= 0 = unit : units
    | isAlive unit = healUnit 1 unit : healOne units (healLeft - 1)
    | otherwise = unit : healOne units healLeft

isAlive :: Unit -> Bool
isAlive (E (Alive _)) = True
isAlive (M (Alive _)) = True
isAlive _ = False

healUnit :: Health -> Unit -> Unit
healUnit heal (E (Alive (Golem hp))) = E (Alive (Golem (hp + heal)))
healUnit heal (E (Alive (HaskellElemental hp))) = E (Alive (HaskellElemental (hp + heal)))
healUnit heal (M (Alive (Master name hp spell))) = M (Alive (Master name (hp + heal) spell))
healUnit _ unit = unit

battle :: Army -> EnemyArmy -> Maybe Army
battle army enemy
    | over enemy && over army = Nothing
    | over army = Just enemy
    | over enemy = Just army
    | otherwise = battle (formationFix (multiHeal 20 (haskellBlast (fight enemy army)))) (formationFix (fight army enemy))

data OneVOne = Winner String | You Health OneVOne | HaskellMage Health OneVOne deriving Eq

instance Show OneVOne where
    show duel = "<" ++ showDuel duel ++ ">" 

showDuel :: OneVOne -> String
showDuel (Winner x) = "|| Winner " ++ x ++ " ||"
showDuel (You hp next) = "You " ++ show hp ++ "; " ++ showDuel next
showDuel (HaskellMage hp next) = "HaskellMage " ++ show hp ++ "; " ++ showDuel next