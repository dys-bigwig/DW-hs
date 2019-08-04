module Combatants where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map


---------------------------------------------------------------------------
--COMBATANTS

data Player = Player
  { playerStats :: Stats
  , playerEquipment :: Equipment
  , playerSpells :: Spells
  }

data Enemy = Enemy
  { enemyStats :: Stats
  , enemyPattern :: EnemyPattern
  } deriving (Show)

data Combatant = PlayerCombatant Player | EnemyCombatant Enemy


---------------------------------------------------------------------------
--STATS

data Stats = Stats
  { name :: String
  , health :: Int
  , strength :: Int
  , agility :: Int
  , mana :: Int
  , status :: Status
  } deriving (Show)


---------------------------------------------------------------------------
--STATUS

data StatusEffect = SleepStatus | StopStatus deriving (Eq, Ord, Enum, Show)

type Status = Set StatusEffect

addStatus :: StatusEffect -> Status -> Status
addStatus statusEffect status = Set.insert statusEffect status


---------------------------------------------------------------------------
--EQUIPMENT

data EquipInfo = EquipInfo
  { equipName :: String
  , equipText :: String
  , equipBuff :: Int
  } deriving (Show)

data Sword = BambooPole | Club | CopperSword | HandAxe | BroadSword | FlameSword | ErdrickSword deriving (Eq, Ord, Enum, Show)
data Shield = LeatherShield | IronShield | SilverShield deriving (Eq, Ord, Enum, Show)
data Armour = Clothes | LeatherArmour | ChainMail | HalfPlate | FullPlate | MagicArmour | ErdrickArmour deriving (Eq, Ord, Enum, Show)

data Equipment = Equipment
  { sword :: Sword
  , shield :: Shield
  , armour :: Armour
  } deriving (Show)

---------------------------------------------------------------------------
--SPELLS

data Spell = Heal | Hurt | StopSpell deriving (Eq, Ord, Enum, Show)

data SpellInfo = SpellInfo
  { spellName :: String
  , spellText :: String
  , spellCost :: Int
  } deriving (Show)

spellInfo :: Spell -> SpellInfo
spellInfo Heal = SpellInfo "Heal" "recovers 10~17 HP" 4
spellInfo Hurt = SpellInfo "Hurt" "deals 8~12 damage" 2
spellInfo StopSpell = SpellInfo "StopSpell" "prevents opponent from casting spells" 2

type Spells = Set Spell


---------------------------------------------------------------------------
--ENEMY PATTERNS

data EnemyPattern = Pattern1 | Pattern2 | Pattern3 | Pattern4 | Pattern5 | Pattern6 | Pattern7 | Pattern8 | Pattern9 | Pattern10
                  | Pattern11 | Pattern12 | Pattern13 | Pattern14 | Pattern15 | Pattern16 | Pattern17 | Pattern18 | Pattern19
                  deriving (Eq, Ord, Enum, Show)
