{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Type.Node.Molecule where

import           Type.Instances                ()

import           Database.Bolt.Extras.Template (makeNodeLike)
import           GHC.Generics                  (Generic)


data Molecule = Molecule
    { id        :: Int
    , smiles    :: String
    , iupacName :: String
    , weight    :: Float
    } deriving (Show, Generic)
makeNodeLike ''Molecule


molecules :: [Molecule]
molecules = map (\(i, s, iu, w) -> Molecule i s iu w)
    [ ( 1
      , "COC1=CC2=C(NC=C2CCNC(C)=O)C=C1"
      , "N-[2-(5-Methoxy-1H-indol-3-yl)ethyl]acetamide"
      , 231.28
      )
    , ( 2
      , "CN=C=O"
      , "methylimino(oxo)methane"
      , 57.05
      )
    , ( 3
      , "[O-]S(=O)(=O)[O-].[Cu+2]"
      , "copper;sulfate"
      , 159.61
      )
    , ( 4
      , "COC1=C(C=CC(=C1)C=O)O"
      , "4-hydroxy-3-methoxybenzaldehyde"
      , 152.15
      )
    , ( 5
      , "CN1CCCC1C2=CN=CC=C2.C1=CC=C(C(=C1)C(=O)O)O"
      , "2-hydroxybenzoic acid;3-[(2R)-1-methylpyrrolidin-2-yl]pyridine"
      , 300.35
      )
    , ( 6
      , "CCCC(CCC=CC=CC#CC#CC=CCO)O"
      , "(2E,8E,10E,14R)-heptadeca-2,8,10-trien-4,6-diyne-1,14-diol"
      , 258.35
      )
    , ( 7
      , "CC1C2CC2(CC1=O)C(C)C"
      , "(1S,4R,5R)-4-methyl-1-propan-2-ylbicyclo[3.1.0]hexan-3-one"
      , 152.23
      )
    , ( 8
      , "CC1C2CC2(CC1=O)C(C)C.CC1C2CC2(CC1=O)C(C)C"
      , "(1S,4S,5R)-4-methyl-1-propan-2-ylbicyclo[3.1.0]hexan-3-one;(1S,4S,5S)-4-methyl-1-propan-2-ylbicyclo[3.1.0]hexan-3-one"
      , 304.5
      )
    , ( 9
      , "CC1=C(SC=[N+]1CC2=CN=C(N=C2N)C)CCO"
      , "2-[3-[(4-amino-2-methylpyrimidin-5-yl)methyl]-4-methyl-1,3-thiazol-3-ium-5-yl]ethanol"
      , 265.36
      )
    , ( 10
      , "COC1=C(C=C2C(=C1O)C3C(C(C(C(O3)CO)O)O)OC2=O)O"
      , "(2R,3S,4S,4aR,10bS)-3,4,8,10-tetrahydroxy-2-(hydroxymethyl)-9-methoxy-3,4,4a,10b-tetrahydro-2H-pyrano[3,2-c]isochromen-6-one"
      , 328.27
      )
    , ( 11
      , "C(C(C(C(C(C=O)O)O)O)O)O"
      , "(2R,3S,4R,5R)-2,3,4,5,6-pentahydroxyhexanal"
      , 180.16
      )
    , ( 12
      , "CN(C)CCC1=CNC2=CC=CC=C21"
      , "2-(1H-indol-3-yl)-N,N-dimethylethanamine"
      , 188.27
      )
    , ( 13
      , "CCN(CC)C(=O)C1CN(C2CC3=CNC4=CC=CC(=C34)C2=C1)C"
      , "(6aR,9R)-N,N-diethyl-7-methyl-6,6a,8,9-tetrahydro-4H-indolo[4,3-fg]quinoline-9-carboxamide"
      , 323.4
      )
    , ( 14
      , "CC(CC1=CC(=C(C=C1OC)Br)OC)N"
      , "1-(4-bromo-2,5-dimethoxyphenyl)propan-2-amine"
      , 274.15
      )
    , ( 15
      , "CCCCCC1=CC(=C2C3C=C(CCC3C(OC2=C1)(C)C)C)O"
      , "(6aR,10aR)-6,6,9-trimethyl-3-pentyl-6a,7,8,10a-tetrahydrobenzo[c]chromen-1-ol"
      , 314.5
      )
    , ( 16
      , "CCCCCC1=CC(=C(C(=C1)O)C2C=C(CCC2C(=C)C)C)O"
      , "2-[(1R,6R)-3-methyl-6-prop-1-en-2-ylcyclohex-2-en-1-yl]-5-pentylbenzene-1,3-diol"
      , 314.5
      )
    , ( 17
      , "CC(C)CC(C(=O)NC(CCCN=C(N)N)C(=O)NC(CCCCN)C(=O)NC(CC1=CC=C(C=C1)O)C(=O)N2CCCC2C(=O)NC(CCCCN)C(=O)O)NC(=O)C(CC3=CC=CC=C3)NC(=O)CNC(=O)CNC(=O)C(CC4=CC=C(C=C4)O)N"
      , "(2S)-6-amino-2-[[(2S)-1-[(2S)-2-[[(2S)-6-amino-2-[[(2S)-2-[[(2S)-2-[[(2S)-2-[[2-[[2-[[(2S)-2-amino-3-(4-hydroxyphenyl)propanoyl]amino]acetyl]amino]acetyl]amino]-3-phenylpropanoyl]amino]-4-methylpentanoyl]amino]-5-(diaminomethylideneamino)pentanoyl]amino]hexanoyl]amino]-3-(4-hydroxyphenyl)propanoyl]pyrrolidine-2-carbonyl]amino]hexanoic acid"
      , 1228.4
      )
    , ( 18
      , "CCC(C)C(C(=O)NC(C(C)CC)C(=O)NC(CCCCN)C(=O)NC(CC(=O)N)C(=O)NC(C)C(=O)NC(CC1=CC=C(C=C1)O)C(=O)NC(CCCCN)C(=O)NC(CCCCN)C(=O)NCC(=O)NC(CCC(=O)N)C(=O)O)NC(=O)C(C)NC(=O)C(CC(=O)N)NC(=O)C(CCCCN)NC(=O)C(CC2=CC=CC=C2)NC(=O)C(CC(C)C)NC(=O)C(C(C)O)NC(=O)C(C(C)C)NC(=O)C(CC(C)C)NC(=O)C3CCCN3C(=O)C(C(C)O)NC(=O)C(CCC(=O)N)NC(=O)C(CO)NC(=O)C(CCCCN)NC(=O)C(CCC(=O)N)NC(=O)C(CO)NC(=O)C(C(C)O)NC(=O)C(CCSC)NC(=O)C(CC4=CC=CC=C4)NC(=O)CNC(=O)CNC(=O)C(CC5=CC=C(C=C5)O)N"
      , "(2S)-5-amino-2-[[2-[[(2S)-6-amino-2-[[(2S)-6-amino-2-[[(2S)-2-[[(2S)-2-[[(2S)-4-amino-2-[[(2S)-6-amino-2-[[(2S,3S)-2-[[(2S,3S)-2-[[(2S)-2-[[(2S)-4-amino-2-[[(2S)-6-amino-2-[[(2S)-2-[[(2S)-2-[[(2S,3R)-2-[[(2S)-2-[[(2S)-2-[[(2S)-1-[(2S,3R)-2-[[(2S)-5-amino-2-[[(2S)-2-[[(2S)-6-amino-2-[[(2S)-5-amino-2-[[(2S)-2-[[(2S,3R)-2-[[(2S)-2-[[(2S)-2-[[2-[[2-[[(2S)-2-amino-3-(4-hydroxyphenyl)propanoyl]amino]acetyl]amino]acetyl]amino]-3-phenylpropanoyl]amino]-4-methylsulfanylbutanoyl]amino]-3-hydroxybutanoyl]amino]-3-hydroxypropanoyl]amino]-5-oxopentanoyl]amino]hexanoyl]amino]-3-hydroxypropanoyl]amino]-5-oxopentanoyl]amino]-3-hydroxybutanoyl]pyrrolidine-2-carbonyl]amino]-4-methylpentanoyl]amino]-3-methylbutanoyl]amino]-3-hydroxybutanoyl]amino]-4-methylpentanoyl]amino]-3-phenylpropanoyl]amino]hexanoyl]amino]-4-oxobutanoyl]amino]propanoyl]amino]-3-methylpentanoyl]amino]-3-methylpentanoyl]amino]hexanoyl]amino]-4-oxobutanoyl]amino]propanoyl]amino]-3-(4-hydroxyphenyl)propanoyl]amino]hexanoyl]amino]hexanoyl]amino]acetyl]amino]-5-oxopentanoic acid"
      , 3463.0
      )
    , ( 19
      , "CC(C)CC(C(=O)NC(C)C(=O)NC(CC(=O)N)C(=O)NC(CCC(=O)N)C(=O)O)NC(=O)C(CCCCN)NC(=O)C(CCCNC(=N)N)NC(=O)C(C)NC(=O)C(CO)NC(=O)C(CCCCN)NC(=O)C(CCCNC(=N)N)NC(=O)C(C)NC(=O)CNC(=O)C(C(C)O)NC(=O)C(CC1=CC=CC=C1)NC(=O)CNC(=O)CNC(=O)C(CC2=CC=CC=C2)N.C(=O)(C(F)(F)F)O"
      , "(2S)-5-amino-2-[[(2S)-4-amino-2-[[(2S)-2-[[(2S)-2-[[(2S)-6-amino-2-[[(2S)-2-[[(2S)-2-[[(2S)-2-[[(2S)-6-amino-2-[[(2S)-2-[[(2S)-2-[[2-[[(2S,3R)-2-[[(2S)-2-[[2-[[2-[[(2S)-2-amino-3-phenylpropanoyl]amino]acetyl]amino]acetyl]amino]-3-phenylpropanoyl]amino]-3-hydroxybutanoyl]amino]acetyl]amino]propanoyl]amino]-5-carbamimidamidopentanoyl]amino]hexanoyl]amino]-3-hydroxypropanoyl]amino]propanoyl]amino]-5-carbamimidamidopentanoyl]amino]hexanoyl]amino]-4-methylpentanoyl]amino]propanoyl]amino]-4-oxobutanoyl]amino]-5-oxopentanoic acid;2,2,2-trifluoroacetic acid"
      , 1923.1
      )
    , ( 20
      , "C1CC(N(C1)C(=O)C(CC2=CC=C(C=C2)O)N)C(=O)NC(CC3=CNC4=CC=CC=C43)C(=O)NC(CC5=CC=CC=C5)C(=O)N.C(=O)(C(F)(F)F)O"
      , "(2S)-1-[(2S)-2-amino-3-(4-hydroxyphenyl)propanoyl]-N-[(2S)-1-[[(2S)-1-amino-1-oxo-3-phenylpropan-2-yl]amino]-3-(1H-indol-3-yl)-1-oxopropan-2-yl]pyrrolidine-2-carboxamide;2,2,2-trifluoroacetic acid"
      , 724.7
      )
    , ( 21
      , "1C=C)O=)C(CNCC2C=CN(C=2CC=1COC"
      , "edimateca]lyhte)ly-3-lodni-H1-yxohteM-5(-2[-N"
      , 28.231
      )
    , ( 22
      , "O=C=NC"
      , "enahtem)oxo(onimilyhtem"
      , 5.57
      )
    , ( 23
      , "]2+uC[.]-O[)O=()O=(S]-O["
      , "etaflus;reppoc"
      , 61.159
      )
    , ( 24
      , "O)O=C)1C=(CC=C(C=1COC"
      , "edyhedlaznebyxohtem-3-yxordyh-4"
      , 15.152
      )
    , ( 25
      , "O)O)O=(C)1C=(C(C=CC=1C.2C=CC=NC=2C1CCCC1NC"
      , "enidiryp]ly-2-nidilorryplyhtem-1-)R2([-3;dica cioznebyxordyh-2"
      , 35.300
      )
    , ( 26
      , "O)OCC=CC#CC#CC=CC=CCC(CCCC"
      , "loid-41,1-enyid-6,4-neirt-01,8,2-acedatpeh-)R41,E01,E8,E2("
      , 35.258
      )
    , ( 27
      , "C)C(C)O=1CC(2CC2C1CC"
      , "eno-3-naxeh]0.1.3[olcycibly-2-naporp-1-lyhtem-4-)R5,R4,S1("
      , 23.152
      )
    , ( 28
      , "C)C(C)O=1CC(2CC2C1CC.C)C(C)O=1CC(2CC2C1CC"
      , "eno-3-naxeh]0.1.3[olcycibly-2-naporp-1-lyhtem-4-)S5,S4,S1(;eno-3-naxeh]0.1.3[olcycibly-2-naporp-1-lyhtem-4-)R5,S4,S1("
      , 5.304
      )
    , ( 29
      , "OCC)C)N2C=N(C=NC=2CC1]+N[=CS(C=1CC"
      , "lonahte]ly-5-mui-3-lozaiht-3,1-lyhtem-4-]lyhtem)ly-5-nidimiryplyhtem-2-onima-4([-3[-2"
      , 36.265
      )
    , ( 30
      , "O)O=2CO)O)O)OC)3O(C(C(C(C3C)O1C=(C2C=C(C=1COC"
      , "eno-6-nemorhcosi]c-2,3[onaryp-H2-ordyhartet-b01,a4,4,3-yxohtem-9-)lyhtemyxordyh(-2-yxordyhartet-01,8,4,3-)Sb01,Ra4,S4,S3,R2("
      , 37.328
      )
    , ( 31
      , "O)O)O)O)O)O=C(C(C(C(C(C"
      , "lanaxehyxordyhatnep-6,5,4,3,2-)R5,R4,S3,R2("
      , 16.180
      )
    , ( 32
      , "12C=CC=CC=2CNC=1CCC)C(NC"
      , "enimanahtelyhtemid-N,N-)ly-3-lodni-H1(-2"
      , 188.27
      )
    , ( 33
      , "C)1C=2C)43C=(CC=CC=4CNC=3CC2C(NC1C)O=(C)CC(NCC"
      , "edimaxobrac-9-eniloniuq]gf-3,4[olodni-H4-ordyhartet-9,8,a6,6-lyhtem-7-lyhteid-N,N-)R9,Ra6("
      , 4.323
      )
    , ( 34
      , "N)CO)rB)CO1C=C(C=(CC=1CC(CC"
      , "enima-2-naporp)lynehpyxohtemid-5,2-omorb-4(-1"
      , 15.274
      )
    , ( 35
      , "O)C)C)C()1C=2CO(C3CCC(C=C3C2C=(CC=1CCCCCC"
      , "lo-1-nemorhc]c[oznebordyhartet-a01,8,7,a6-lytnep-3-lyhtemirt-9,6,6-)Ra01,Ra6("
      , 5.314
      )
    , ( 36
      , "O)C)C)C=(C2CCC(C=C2C)O)1C=(C(C=(CC=1CCCCCC"
      , "loid-3,1-enezneblytnep-5-]ly-1-ne-2-xeholcycly-2-ne-1-porp-6-lyhtem-3-)R6,R1([-2"
      , 5.314
      )
    , ( 37
      , "N)O)4C=C(C=CC=4CC(C)O=(CNC)O=(CNC)O=(CN)3C=CC=CC=3CC(C)O=(CN)O)O=(C)NCCCC(CN)O=(C2CCCC2N)O=(C)O)1C=C(C=CC=1CC(CN)O=(C)NCCCC(CN)O=(C)N)N(C=NCCC(CN)O=(C(CC)C(CC"
      , "dica cionaxeh]onima]lynobrac-2-enidilorryp]lyonaporp)lynehpyxordyh-4(-3-]onima]lyonaxeh]onima]lyonatnep)onimaenedilyhtemonimaid(-5-]onima]lyonatneplyhtem-4-]onima]lyonaporplynehp-3-]onima]lyteca]onima]lyteca]onima]lyonaporp)lynehpyxordyh-4(-3-onima-2-)S2([[-2[[-2[[-2-)S2([[-2-)S2([[-2-)S2([[-2-onima-6-)S2([[-2-)S2([-1-)S2([[-2-onima-6-)S2("
      , 4.1228
      )
    , ( 38
      , "N)O)5C=C(C=CC=5CC(C)O=(CNC)O=(CNC)O=(CN)4C=CC=CC=4CC(C)O=(CN)CSCC(C)O=(CN)O)C(C(C)O=(CN)OC(C)O=(CN)N)O=(CCC(C)O=(CN)NCCCC(C)O=(CN)OC(C)O=(CN)N)O=(CCC(C)O=(CN)O)C(C(C)O=(C3NCCC3C)O=(CN)C)C(CC(C)O=(CN)C)C(C(C)O=(CN)O)C(C(C)O=(CN)C)C(CC(C)O=(CN)2C=CC=CC=2CC(C)O=(CN)NCCCC(C)O=(CN)N)O=(CC(C)O=(CN)C(C)O=(CN)O)O=(C)N)O=(CCC(CN)O=(CCN)O=(C)NCCCC(CN)O=(C)NCCCC(CN)O=(C)O)1C=C(C=CC=1CC(CN)O=(C)C(CN)O=(C)N)O=(CC(CN)O=(C)NCCCC(CN)O=(C)CC)C(C(CN)O=(C(C)C(CCC"
      , "dica cionatnepoxo-5-]onima]lyteca]onima]lyonaxeh]onima]lyonaxeh]onima]lyonaporp)lynehpyxordyh-4(-3-]onima]lyonaporp]onima]lyonatuboxo-4-]onima]lyonaxeh]onima]lyonatneplyhtem-3-]onima]lyonatneplyhtem-3-]onima]lyonaporp]onima]lyonatuboxo-4-]onima]lyonaxeh]onima]lyonaporplynehp-3-]onima]lyonatneplyhtem-4-]onima]lyonatubyxordyh-3-]onima]lyonatublyhtem-3-]onima]lyonatneplyhtem-4-]onima]lynobrac-2-enidilorryp]lyonatubyxordyh-3-]onima]lyonatnepoxo-5-]onima]lyonaporpyxordyh-3-]onima]lyonaxeh]onima]lyonatnepoxo-5-]onima]lyonaporpyxordyh-3-]onima]lyonatubyxordyh-3-]onima]lyonatublynafluslyhtem-4-]onima]lyonaporplynehp-3-]onima]lyteca]onima]lyteca]onima]lyonaporp)lynehpyxordyh-4(-3-onima-2-)S2([[-2[[-2[[-2-)S2([[-2-)S2([[-2-)R3,S2([[-2-)S2([[-2-onima-5-)S2([[-2-onima-6-)S2([[-2-)S2([[-2-onima-5-)S2([[-2-)R3,S2([-1-)S2([[-2-)S2([[-2-)S2([[-2-)R3,S2([[-2-)S2([[-2-)S2([[-2-onima-6-)S2([[-2-onima-4-)S2([[-2-)S2([[-2-)S3,S2([[-2-)S3,S2([[-2-onima-6-)S2([[-2-onima-4-)S2([[-2-)S2([[-2-)S2([[-2-onima-6-)S2([[-2-onima-6-)S2([[-2[[-2-onima-5-)S2("
      , 0.3463
      )
    , ( 39
      , "O)F)F()F(C()O=(C.N)2C=CC=CC=2CC(C)O=(CNC)O=(CNC)O=(CN)1C=CC=CC=1CC(C)O=(CN)O)C(C(C)O=(CNC)O=(CN)C(C)O=(CN)N)N=(CNCCC(C)O=(CN)NCCCC(C)O=(CN)OC(C)O=(CN)C(C)O=(CN)N)N=(CNCCC(C)O=(CN)NCCCC(C)O=(CN)O)O=(C)N)O=(CCC(CN)O=(C)N)O=(CC(CN)O=(C)C(CN)O=(C(CC)C(CC"
      , "dica citecaoroulfirt-2,2,2;dica cionatnepoxo-5-]onima]lyonatuboxo-4-]onima]lyonaporp]onima]lyonatneplyhtem-4-]onima]lyonaxeh]onima]lyonatnepodimadimimabrac-5-]onima]lyonaporp]onima]lyonaporpyxordyh-3-]onima]lyonaxeh]onima]lyonatnepodimadimimabrac-5-]onima]lyonaporp]onima]lyteca]onima]lyonatubyxordyh-3-]onima]lyonaporplynehp-3-]onima]lyteca]onima]lyteca]onima]lyonaporplynehp-3-onima-2-)S2([[-2[[-2[[-2-)S2([[-2-)R3,S2([[-2[[-2-)S2([[-2-)S2([[-2-onima-6-)S2([[-2-)S2([[-2-)S2([[-2-)S2([[-2-onima-6-)S2([[-2-)S2([[-2-)S2([[-2-onima-4-)S2([[-2-onima-5-)S2("
      , 1.1923
      )
    , ( 40
      , "O)F)F()F(C()O=(C.N)O=(C)5C=CC=CC=5CC(CN)O=(C)34C=CC=CC=4CNC=3CC(CN)O=(C)N)O)2C=C(C=CC=2CC(C)O=(C)1C(N(CC1C"
      , "dica citecaoroulfirt-2,2,2;edimaxobrac-2-enidilorryp]ly-2-naporpoxo-1-)ly-3-lodni-H1(-3-]onima]ly-2-naporplynehp-3-oxo-1-onima-1-)S2([[-1-)S2([-N-]lyonaporp)lynehpyxordyh-4(-3-onima-2-)S2([-1-)S2("
      , 7.724
      )
    ]
