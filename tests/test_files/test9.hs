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

instance Show Mage where
    show (Master name hp sp)
        | hp < 5 = "Wounded " ++ name
        | otherwise = name

instance Eq Mage where
    (==) (Master name1 hp1 sp1) (Master name2 hp2 sp2) = name1 == name2 && hp1 == hp2

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

data Unit = M (State Mage) | E (State Entity)

instance Show Unit where
    show (M state) = showMage state
    show (E state) = show state

instance Eq Unit where
    (==) (M m1) (M m2) = m1 == m2
    (==) (E e1) (E e2) = e1 == e2
    (==) _ _ = False

formationFix :: Army -> Army
formationFix a = helper a [] where
    helper :: Army -> Army -> Army
    helper [] dead = reverse dead
    helper (u@(E Dead):xs) dead = helper xs (u : dead)
    helper (u@(M Dead):xs) dead = helper xs (u : dead)
    helper (x:xs) dead = x : helper xs dead

over :: Army -> Bool
over [] = True
over ((E Dead):xs) = over xs
over ((M Dead):xs) = over xs
over (_:xs) = False

fight :: EnemyArmy -> Army -> Army
fight xs ys = helper xs ys [] where
    helper :: EnemyArmy -> Army -> [Spell] -> Army
    helper _ [] _ = []
    helper [] (ally:a) spells = damageAllSpells spells ally : helper [] a spells
    helper enemy ((E Dead):a) spells = (E Dead) : helper enemy a spells
    helper enemy ((M Dead):a) spells = (M Dead) : helper enemy a spells
    helper (enemy@(E _):ea) (ally:a) spells = doDamage (getDamageE enemy) (damageAllSpells spells ally) : helper ea a spells
    helper (mage@(M _):ea) (ally:a) spells = damageAllSpells ((getSpell mage) : spells) ally : helper ea a ((getSpell mage) : spells)

getDamageE :: Unit -> Amount
getDamageE (E Dead) = 0
getDamageE (E (Alive (Golem _))) = 1
getDamageE (E (Alive (HaskellElemental _))) = 3

doDamage :: Amount -> Unit -> Unit
doDamage _ (E Dead) = (E Dead)
doDamage _ (M Dead) = (M Dead)
doDamage d (E (Alive (Golem hp)))
    | hp - d < 1 = (E Dead)
    | otherwise = (E (Alive (Golem (hp - d))))
doDamage d (E (Alive (HaskellElemental hp)))
    | hp - d < 1 = (E Dead)
    | otherwise = (E (Alive (HaskellElemental (hp - d))))
doDamage d (M (Alive (Master name hp spell)))
    | hp - d < 1 = (M Dead)
    | otherwise = (M (Alive (Master name (hp - d) spell)))

getSpell :: Unit -> Spell
getSpell (M Dead) = id
getSpell (M (Alive (Master _ _ spell))) = spell

damageAllSpells :: [Spell] -> Unit -> Unit
damageAllSpells [] ally = ally
damageAllSpells _ (E Dead) = E Dead
damageAllSpells _ (M Dead) = M Dead
damageAllSpells (spell:sp) (E (Alive (Golem hp)))
    | spell hp < 1 = (E Dead)
    | otherwise = damageAllSpells sp (E (Alive (Golem (spell hp))))
damageAllSpells (spell:sp) (E (Alive (HaskellElemental hp)))
    | spell hp < 1 = (E Dead)
    | otherwise = damageAllSpells sp (E (Alive (HaskellElemental (spell hp))))
damageAllSpells (spell:sp) (M (Alive (Master name hp s)))
    | spell hp < 1 = (M Dead)
    | otherwise = damageAllSpells sp (M (Alive (Master name (spell hp) s)))

haskellBlast :: Army -> Army
haskellBlast [] = []
haskellBlast army
    | length army < 5 = damageNext5 army
    | maximum (getAllSum army) == 0 = doMostDamage army (getAllSumZero army)
    | maximum (getAllSum army) >= 25 = findAndDamage army (getAllSum army)
    | otherwise = doMostDamage army (getAllSum army)

getHealth :: Unit -> Health
getHealth (E Dead) = 0
getHealth (E (Alive (Golem hp))) = hp
getHealth (E (Alive (HaskellElemental hp))) = hp
getHealth (M Dead) = 0
getHealth (M (Alive (Master _ hp _))) = hp

sumHealth :: Army -> Health
sumHealth [] = 0
sumHealth (ally:army) = getHealth ally + sumHealth army

getAllSum :: Army -> [Health]
getAllSum [] = []
getAllSum army
    | any (<5) (take 5 (map getHealth army)) = 0 : getAllSum (tail army)
    | otherwise = sumHealth (take 5 army) : getAllSum (tail army) 

damageNext5 :: Army -> Army
damageNext5 army = helper army 0 where
    helper :: Army -> Int -> Army
    helper [] _ = []
    helper (ally:army) counter 
        | counter == 5 = (ally:army)
        | otherwise = doDamage 5 ally : helper army (counter + 1)

findAndDamage :: Army -> [Health] -> Army
findAndDamage [] _ = []
findAndDamage (ally:a) (h:hs)
    | h >= 25 = damageNext5 (ally:a)
    | otherwise = ally : findAndDamage a hs

doMostDamage :: Army -> [Health] -> Army
doMostDamage [] _ = []
doMostDamage army@(ally:a) (h:hs)
    | h == maximum (getAllSumZero army) = damageNext5 (ally:a)
    | otherwise = ally : doMostDamage a hs

getAllSumZero :: Army -> [Health]
getAllSumZero [] = []
getAllSumZero army = sumHealth (take 5 army) : getAllSumZero (tail army)

multiHeal :: Health -> Army -> Army
multiHeal h [] = []
multiHeal h army
    | h < 0 = army
    | over army = army
    | otherwise = helper h army [] where
        helper :: Health -> Army -> Army -> Army
        helper h [] healed = multiHeal h (reverse healed)
        helper 0 army healed = reverse healed ++ army
        helper h ((E Dead):army) healed = helper h army ((E Dead) : healed)
        helper h ((M Dead):army) healed = helper h army ((M Dead) : healed)
        helper h (ally:army) healed = helper (h - 1) army ((updateHp 1 ally) : healed)

updateHp :: Health -> Unit -> Unit
updateHp _ (E Dead) = E Dead
updateHp _ (M Dead) = M Dead
updateHp h (E (Alive (Golem hp))) = (E (Alive (Golem (hp + h))))
updateHp h (E (Alive (HaskellElemental hp))) = (E (Alive (HaskellElemental (hp + h))))
updateHp h (M (Alive (Master name hp spell))) = (M (Alive (Master name (hp + h) spell)))

isDead :: Unit -> Bool
isDead (E Dead) = True
isDead (M Dead) = True
isDead _ = False

battle :: Army -> EnemyArmy -> Maybe Army
battle a ea
    | over a && over ea = Nothing
    | over a && not (over ea) = Just ea
    | not (over a) && over ea = Just a
    | otherwise = battle (formationFix (multiHeal 20 (haskellBlast (fight ea a)))) (formationFix (fight a ea))

data OneVOne = Winner String | You Health OneVOne | HaskellMage Health OneVOne deriving Eq

instance Show OneVOne where
    show (Winner x) = "<|| Winner " ++ x ++ " ||>"
    show x = "<" ++ showOther x ++ ">" where
        showOther :: OneVOne -> String
        showOther (Winner x) = "|| Winner " ++ x ++ " ||"
        showOther (You hp rest) = "You " ++ show hp ++ "; " ++ showOther rest
        showOther (HaskellMage hp rest) = "HaskellMage " ++ show hp ++ "; " ++ showOther rest

finalBattle :: Health -> Health -> OneVOne
finalBattle me hm
    | hm <= 0 = HaskellMage 0 (Winner "You")
    | me <= 0 = Winner "HaskellMage"
    | otherwise = (HaskellMage hm (You (fst hps) (finalBattle (fst (getMyDamage hps)) (snd (getMyDamage hps))))) where hps = getHMDamage (me, hm) 

getHMDamage :: (Health, Health) -> (Health, Health)
getHMDamage (me, hm)
    | hm < 4 = (div me 2, 4 * hm)
    | hm >= 4 && me > 20 = (div (3 * me) 4, hm)
    | otherwise = (zeroIfNeg (me - 11), hm)

getMyDamage :: (Health, Health) -> (Health, Health)
getMyDamage (me, hm)
    | me < 4 = (4 * me, hm)
    | hm > 15 = (me, div (3 * hm) 5)
    | otherwise = (me, zeroIfNeg (hm - 9))

zeroIfNeg :: Health -> Health
zeroIfNeg h
    | h < 0 = 0
    | otherwise = h