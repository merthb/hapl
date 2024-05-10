module Beadandó where

showState a=show a
showMage a=show a
eqMage a b=a==b
showUnit a=show a
showOneVOne a=show a

type Name=String
type Health=Integer
type Spell=(Integer->Integer)
type Army=[Unit]
type EnemyArmy=Army
type Amount=Integer

data State a=Alive a|Dead
    deriving (Eq)

instance Show a=>Show (State a) where
    show (Alive x)=show x
    show Dead="Dead"

data Entity=Golem Health|HaskellElemental Health
    deriving (Show,Eq)

data Mage=Master Name Health Spell

instance Show Mage where
    show (Master x y z)
        |y>=5=x
        |otherwise="Wounded "++x

instance Eq Mage where
    (==) (Master x y z) (Master a b c)=(x==a)&&(y==b)

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

data Unit=M (State Mage)|E (State Entity)
    deriving (Eq)

instance Show Unit where
    show (M x)=show x
    show (E x)=show x

formationFix::Army->Army
formationFix []=[]
formationFix (x:xs)=getAlive (x:xs)++getDead (x:xs)
getDead::Army->Army
getDead []=[]
getDead (x:xs)
    |x==(E Dead)||x==(M Dead)=x:getDead xs
    |otherwise=getDead xs
getAlive::Army->Army
getAlive []=[]
getAlive (y@(M (Alive x)):xs)=y:getAlive xs
getAlive (y@(E (Alive x)):xs)=y:getAlive xs
getAlive (M Dead:xs)=getAlive xs
getAlive (E Dead:xs)=getAlive xs

over::Army->Bool
over []=True
over (x:xs)
    |x==(E Dead)||x==(M Dead)=over xs
    |otherwise=False

potionMaster = 
  let plx x
        | x > 85  = x - plx (div x 2)
        | x == 60 = 31
        | x >= 51 = 1 + mod x 30
        | otherwise = x - 7 
  in Master "PotionMaster" 170 plx

fight::EnemyArmy->Army->Army
fight [] xs=xs
fight _ []=[]
fight (E (Alive (Golem _)):xs) (y:ys)=[damage y 1]++fight xs ys
fight (E (Alive (HaskellElemental _)):xs) (y:ys)=[damage y 3]++fight xs ys
fight (M (Alive (Master a b c)):xs) (y:ys)=[mageDamageOne c y]++fight xs (mageDamage ys c)
fight ((E Dead):xs) ys=[damage (head ys) 0]++fight xs (tail ys)
fight ((M Dead):xs) ys=[damage (head ys) 0]++fight xs (tail ys)
damage::Unit->Integer->Unit
damage (E Dead) _=(E Dead)
damage (M Dead) _=(M Dead)
damage (M (Alive (Master x y z))) d
    |d<=0=(M (Alive (Master x y z)))
    |d>=y=(M Dead)
    |otherwise=(M (Alive (Master x (y-d) z)))
damage (E (Alive (Golem y))) d
    |d<=0=(E (Alive (Golem y)))
    |d>=y=(E Dead)
    |otherwise=(E (Alive (Golem (y-d))))
damage (E (Alive (HaskellElemental y))) d
    |d<=0=(E (Alive (HaskellElemental y)))
    |d>=y=(E Dead)
    |otherwise=(E (Alive (HaskellElemental (y-d))))
mageDamageOne::Spell->Unit->Unit
mageDamageOne _ (E Dead)=(E Dead)
mageDamageOne _ (M Dead)=(M Dead)
mageDamageOne s (E (Alive (Golem x)))
    |(s x)<=0=(E Dead)
    |otherwise=(E (Alive (Golem (s x))))
mageDamageOne s (E (Alive (HaskellElemental x)))
    |(s x)<=0=(E Dead)
    |otherwise=(E (Alive (HaskellElemental (s x))))
mageDamageOne s (M (Alive (Master a x y)))
    |(s x)<=0=(M Dead)
    |otherwise=(M (Alive (Master a (s x) y)))
mageDamage::Army->Spell->Army
mageDamage [] _=[]
mageDamage (x:xs) s=cast s x:mageDamage xs s
cast::Spell->Unit->Unit
cast _ (E Dead)=(E Dead)
cast _ (M Dead)=(M Dead)
cast a (M (Alive (Master x y z)))
    |(a y)<=0=(M Dead)
    |otherwise=(M (Alive (Master x (a y) z)))
cast a (E (Alive (Golem y)))
    |(a y)<=0=(E Dead)
    |otherwise=(E (Alive (Golem (a y))))
cast a (E (Alive (HaskellElemental y)))
    |(a y)<=0=(E Dead)
    |otherwise=(E (Alive (HaskellElemental (a y))))

haskellBlast::Army->Army
haskellBlast []=[]
haskellBlast (x:xs)=blastDamage (x:xs) (5+(maxBlastDamage (getPosition (possibleBlastDamages (x:xs)))))
blastDamage::Army->Integer->Army
blastDamage xs 0=xs
blastDamage [] _=[]
blastDamage (x:xs) n
    |n<=5=(damage x 5):blastDamage xs (n-1)
    |otherwise=x:blastDamage xs (n-1)
checkBlastDamage::Army->Army->Integer
checkBlastDamage [] []=0
checkBlastDamage (x:xs) (y:ys)
    |(x:xs)==(y:ys)=0
    |otherwise=healthDifference x y+checkBlastDamage xs ys
healthDifference::Unit->Unit->Integer
healthDifference (E Dead) _=0
healthDifference (M Dead) _=0
healthDifference (E (Alive (Golem x))) (E Dead)=x
healthDifference (E (Alive (HaskellElemental x))) (E Dead)=x
healthDifference (M (Alive (Master _ x _))) (M Dead)=x
healthDifference (E (Alive (Golem x))) (E (Alive (Golem y)))=x-y
healthDifference (E (Alive (HaskellElemental x))) (E (Alive (HaskellElemental y)))=x-y
healthDifference (M (Alive (Master _ x _))) (M (Alive (Master _ y _)))=x-y
healthDifference _ _=error "Így ne legyen meghívva healthDifference"
maxBlastDamage::[(Integer,Integer)]->Integer
maxBlastDamage [a]=(snd a)
maxBlastDamage (x:y:xs)
    |(fst y)>(fst x)=maxBlastDamage (y:xs)
    |otherwise=maxBlastDamage (x:xs)
possibleBlastDamages::Army->[Integer]
possibleBlastDamages []=[0]
possibleBlastDamages [x]=[checkBlastDamage [x] [damage x 5]]
possibleBlastDamages [x,y]=[checkBlastDamage [x,y] (damageFive [x,y] 2)]
possibleBlastDamages [x,y,z]=[checkBlastDamage [x,y,z] (damageFive [x,y,z] 3)]
possibleBlastDamages [x,y,z,a]=[checkBlastDamage [x,y,z,a] (damageFive [x,y,z,a] 4)]
possibleBlastDamages (x:xs)
    |length(x:xs)>=5=[checkBlastDamage (x:xs) (damageFive (x:xs) 5)]++possibleBlastDamages xs
    |otherwise=[checkBlastDamage (x:xs) $ damageFive (x:xs) 4]
getPosition::[Integer]->[(Integer,Integer)]
getPosition xs=zip xs [0..]
alive::Army->Integer->Bool
alive [] 0=True
alive [] x=False
alive (x:xs) n
    |x==(E Dead)||x==(M Dead)=False
    |otherwise=True&&alive xs (n-1)
firstAlive::Army->Army
firstAlive []=[]
firstAlive (x:xs)
    |x/=(E Dead)&&x/=(M Dead)=x:firstAlive xs
    |otherwise=[x]
damageFive::Army->Integer->Army
damageFive [] _=[]
damageFive xs 0=xs
damageFive (x:xs) n=damage x 5:damageFive xs (n-1)

multiHeal::Health->Army->Army
multiHeal _ []=[]
multiHeal n (x:xs)
    |n<=0=(x:xs)
    |x==(E Dead)=(E Dead):multiHeal n xs
    |x==(M Dead)=(M Dead):multiHeal n xs
    |tooLong n (getAlive (x:xs))=healOne 1 (x):multiHeal (n-1) xs
    |x/=(E Dead)&&x/=(M Dead)=healOne (getHealAmount n (x:xs)) x:multiHeal (n-(getHealAmount n (x:xs))) xs
healOne::Health->Unit->Unit
healOne n (E (Alive (Golem x)))=(E (Alive (Golem (n+x))))
healOne n (E (Alive (HaskellElemental x)))=(E (Alive (HaskellElemental (n+x))))
healOne n (M (Alive (Master a x b)))=(M (Alive (Master a (n+x) b)))
healOne _ (E Dead)=(E Dead)
healOne _ (M Dead)=(M Dead)
getHealAmount::Integer->Army->Integer
getHealAmount n xs
    |n`mod`(fromIntegral (length (getAlive xs)))/=0=n`div`(fromIntegral (length (getAlive xs)))+1
    |otherwise=n`div`(fromIntegral (length (getAlive xs)))
tooLong::Integer->[a]->Bool
tooLong _ []=False
tooLong 0 (xs)=True
tooLong n (x:xs)=tooLong (n-1) xs

battle::Army->EnemyArmy->Maybe Army
battle xs ys
    |(over xs)&&(over ys)=Nothing
    |(over xs)=Just ys
    |(over ys)=Just xs
    |otherwise=battle (formationFix (multiHeal 20 (haskellBlast (fight ys xs)))) (formationFix (fight xs ys))

data OneVOne=Winner String|You Health OneVOne|HaskellMage Health OneVOne
    deriving Eq

instance Show OneVOne where
    show (Winner x)="<|| Winner "++x++" ||>"
    show x="<"++showVS x++">" where
        showVS (Winner x)="|| Winner "++x++" ||"
        showVS (You x y)="You "++show x++"; "++showVS y
        showVS (HaskellMage x y)="HaskellMage "++show x++"; "++showVS y

finalBattle::Health->Health->OneVOne
finalBattle x y=oneTurn x y "HaskellMage" where
    oneTurn::Health->Health->String->OneVOne
    oneTurn x y z
        |y<=0=(HaskellMage 0 (Winner "You"))
        |x<=0=(You 0 (Winner "HaskellMage"))
        |z=="HaskellMage"&&y<4=(HaskellMage y (oneTurn (x`div`2) (y*4) "You"))
        |z=="HaskellMage"&&x>20=(HaskellMage y (oneTurn ((x*3)`div`4) y "You"))
        |z=="HaskellMage"=(HaskellMage y (oneTurn (x-11) y "You"))
        |z=="You"&&x<4=(You x (oneTurn (x*4) y "HaskellMage"))
        |z=="You"&&y>15=(You x (oneTurn x ((y*3)`div`5) "HaskellMage"))
        |z=="You"=(You x (oneTurn x (y-9) "HaskellMage"))
        |otherwise= error "What. How."