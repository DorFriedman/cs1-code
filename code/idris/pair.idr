module pair

data pair a b = mkPair a b

fst: pair a b -> a
fst (mkPair f s) = f

snd: pair a b -> b
snd (mkPair f s) = s
