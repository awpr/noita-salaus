{-# LANGUAGE DataKinds #-}

module Data where

import Data.Char (ord)
import Data.Foldable (fold)

import Salaus.Symbol

type Trigram = Symbol 83
type EyeMessage = [Trigram]

-- Decode the 32-based ASCII embedding into trigram values.
unmap :: String -> EyeMessage
unmap = map (intToSymbol . subtract 32 . ord)

m0, m1, m2, m3, m4, m5, m6, m7, m8 :: EyeMessage

m0 = unmap "Rb%P^-k=8]Jfb^@.q(/n\"=-Q!prH_q53 HSa:.5fOLPJ3P-O3Qh?%8#K[cAQI\\5:>%94g+jX$j3g$SIKphV_oq/0L?>,AY<-`KP"

m1 = unmap "pb%P^-k=8]Jfb^@.q(/n\"=-Q!=+>Tq53 9:V4.5fOLPJ3P-O3QL:[m`Ko<h`!>i7c&A9`qdN1D-15d-)NcYB^r/*i^\"+ahEL*Kd^)B2"

m2 = unmap "Db%P^-k=8]Jfb^@.q(/n\"=-Q!elT)Pbp6`YHQn#0X3OHp&-`=Q`_&Q?-0*M8:m*\\q]BVf5/$bmJE>6 +IhY47YaI72hJ%#:n(%VMm9`]0LVS4_9+:MU\\FB"

m3 = unmap "lb%QkVeN@!J\\:PRp@8W]O,5,QVB9D/XW4)(^-r)L=\\UrJp%Kg#pmOnB9^2*Q^`Tq+b^-O1Tf:7@?`7C@R&!9(EOK:ladp1'M_.U_\\0"

m4 = unmap "_b%QkV\"\\=HnO\\kcg\\\"a'O.Mj[Ip-\\-q6CRHG\"[P?l\"pk!Xc+5(HaMkWG\\J-#6Y\"&Z)f!ZX_d9o'43`\"bi>g0,>aE4-6_2N`[Iqr6nDO1$&1%Do_!`e/K$ZX?.`Z2Lne! N4gi9C(8"

m5 = unmap "Bb%QkV7j+-<:3PcYE\\B<j*1@+23K3qJ$^)NQ@SlZ$KO1co5@L0>E:<IdYBS*ef(&NK2GOK/-A>C^E E%FWE-H9)5+`%oJd+g+P#c]H6.CR]G+\"bQSU1iDkjV8>Vf"

m6 = unmap ";b%QkV\"\\=H\"W)/[2d#D%OmLF!2<l$B\\_Zp1VokPVW3^`.OSfk%+OMZdeo9FMiOdRBMn:oY$X6\\2kK\\[c_JQAHaom'#:^?n:YeH$7:-cJFh+Ga\\9&pbdm[n3"

m7 = unmap "mb%QkV\"\\=H\"W)/[2d#D%O\\5p!hW0rCY3!b2;G1jqG.n 9aKb`Fq78RY>gk:dVYXRgi.5(@:_%E3KbOUBb7i?VFmc+_o&65Sej5%1cE=5\\.rL>$4JC!?VN4H>"

m8 = unmap "Ab%QkV\"\\=H\"W)/[2d#D%OA5[L2<l[B\\_o;,V%QPVWT^he*Y6ZPcU'B@>?3:(BN'>gWBkV)&\\%79MJp9,6l4S^5H)I*Li(Afi&?5h%H]SJb`j]9_J8I"

messages :: [EyeMessage]
messages = [m0, m1, m2, m3, m4, m5, m6, m7, m8]

corpus :: EyeMessage
corpus = fold messages

english :: String
english = "Doing this produces trigrams that span all possible values from 000 to 312, or 83 unique results (out of a possible 125). This may be the correct way to read them -- other orderings produce gaps in the sequence and more unique combinations.  This spreadsheet contains the resulting trigrams, as well as their conversion from base-5 into base-10.  Translating the base-10 codes into ASCII via simple addition (c + 32) results in the following strings, which are more or less meaningless but may be useful to anyone attempting cryptanalysis:"
