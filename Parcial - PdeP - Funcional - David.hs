-- Punto 1
data UnaPersona = Persona { nombre :: String, nivelSat :: String, nivelEmo ::Int, nivelCult :: String} deriving Show

ana:: UnaPersona
ana = Persona "ana" 10 20 60

ana:: UnaPersona
ana = Persona "ana" 20 30 40


-- Punto 2
type Atraccion = UnaPersona -> UnaPersona

montañaRusa:: Int -> Int -> Atraccion
montañaRusa velocidad altura persona | velocidad > 50 = persona {nivelEmo = nivelEmo persona + (*0,15) velocidad + altura}
                                     | otherwise = persona {nivelEmo = nivelEmo persona - ((*0,05).nivelEmo) persona, nivelSat = nivelSat persona - ((*0,1).nivelSat) persona}

caidaLibre:: Int -> Atraccion
caidaLibre metros persona = persona {nivelEmo = nivelEmo persona + (*0,2) metros}

mundoMaya:: Atraccion
mundoMaya persona = persona {nivelEmo = nivelEmo persona + ((*0,1).nivelEmo) persona, nivelCult = nivelCult persona + ((*0,20).nivelCult) persona}

showDeMagia:: Atraccion
showDeMagia persona | nivelCult persona > 50  = persona {nivelSat = ((+20).nivelSat) persona}
                    | otherwise = persona {nivelEmo = ((+30).nivelEmo) persona}


-- Punto 3
visitar:: [Atraccion] -> UnaPersona -> UnaPersona
visitar atracciones persona = foldl realizarAtraccion persona atracciones

realizarAtraccion:: UnaPersona -> Atraccion -> UnaPersona
realizarAtraccion persona atraccion = atraccion persona


-- Punto 4



-- Punto 5
estanFelices:: [UnaPersona] -> Bool
estanFelices personas = ((all estanSatisfechas).map (montañaRusa ).(filter estaEmocionada)) personas 

estaEmocionada:: UnaPersona -> Bool
estaEmocionada persona = nivelEmo persona > 60

estanSatisfechas:: UnaPersona -> Bool
estanSatisfechas 







