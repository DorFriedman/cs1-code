module colors

import pair
import bool

data color = red | blue | green | mangenta | yellow | cyan

complement: color -> color
complement red = cyan
complement mangenta = green
complement blue = yellow
complement cyan = red
complement green = mangenta
complement yellow = blue

additive: color -> bool
additive red = true
additive blue = true
additive green = true
additive _ = false

subtractive: color -> bool
subtractive b = not (additive b)

complements: pair color color -> bool
complements (mkPair red cyan) = true
complements (mkPair cyan red) = true
complements (mkPair green mangenta) = true
complements (mkPair mangenta green) = true
complements (mkPair blue yellow) = true
complements (mkPair yellow blue) = true
complements (mkPair _ _) = false

mixink: pair color color -> color
mixink (mkPair mangenta cyan) = blue
mixink (mkPair cyan mangenta) = blue
mixink (mkPair mangenta yellow) = red
mixink (mkPair yellow mangenta) = red
mixink (mkPair cyan yellow) = green
mixink (mkPair yellow cyan) = green
