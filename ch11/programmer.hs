data OperatingSystem =
    GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show, Enum)

data ProgLang =
    Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show, Enum)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgLang }
  deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer { os = Mac, lang = Haskell }

feelingWizardly :: Programmer
feelingWizardly = Programmer { lang = Agda, os = GnuPlusLinux }

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux ..]

allLanguages :: [ProgLang]
allLanguages = [Haskell ..]

allProgrammers :: [Programmer]
allProgrammers = [Programmer {os = os', lang = lang'} |
  os' <- allOperatingSystems, lang' <- allLanguages]