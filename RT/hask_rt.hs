{-# LANGUAGE ViewPatterns #-}

import System.Random
import Data.List
import Data.Char
import System.IO  
import Numeric
import Data.Bits
import Data.Bits.Extras
import qualified Data.ByteString.Lazy as BL

data Vec3 = Vec3{x :: Float, y :: Float, z :: Float}

instance Show Vec3 where
    show (Vec3 a b c) = "(" ++ (show a) ++ ", " ++ (show b) ++ ", " ++ (show c) ++")"


instance Num Vec3 where
    (Vec3 x1 y1 z1) + (Vec3 x2 y2 z2) = Vec3 (x1+x2) (y1+y2) (z1+z2)
    (Vec3 x1 y1 z1) - (Vec3 x2 y2 z2) = Vec3 (x1-x2) (y1-y2) (z1-z2)
    (Vec3 x1 y1 z1) * (Vec3 x2 y2 z2) = Vec3 (x1*x2) (y1*y2) (z1*z2)

fVec3 :: Float -> Vec3
fVec3 x = Vec3 x x x

scalMul :: Vec3 -> Vec3 -> Float
scalMul (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = (x1*x2)+(y1*y2)+(z1*z2)

vlen :: Vec3 -> Float
vlen v = sqrt (scalMul v v)

normalize :: Vec3 -> Vec3
normalize (Vec3 x y z) = Vec3 (x/len) (y/len) (z/len)
  where len = vlen (Vec3 x y z)

det :: Float -> Float -> Float -> Float -> Float
det a b c d = (a*d) - (c*b)

vecMul :: Vec3 -> Vec3 -> Vec3
vecMul (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (det y1 y2 z1 z2) (det z1 z2 x1 x2) (det x1 x2 y1 y2)

clamp :: Float -> Float -> Float -> Float
clamp min max x = if (x<min) then min else (if (x>max) then max else x) 

reflect :: Vec3 -> Vec3 -> Vec3
reflect ray normal = ray - ((fVec3 (2.0 * (scalMul ray normal))) * normal)



-- TODO: make this actually random

--randItvlH :: Float -> Float -> RandomGen -> Float
--randItvlH min max gn = randomR (min, max) gn

newRand = randomIO :: IO Float

randItvl :: Float -> Float -> Float
--randItvl min max = do{
--    randVal <- randomIO;
--    remap (0.0, 1.0) (min, max) randVal}
randItvl min max = 0.5 * (min + max)


-- Polar coords
-- z>0 always
randomZAxisHemisphereVec3 :: Float -> Vec3
randomZAxisHemisphereVec3 roughness = let
  z = randItvl (cos (roughness * pi)) 1
  angle = randItvl (-pi) (pi)
  r = sqrt (1.0 - (z * z))
  in Vec3 (r * (sin angle)) (r * (cos angle)) z

changeBase :: Vec3 -> (Vec3, Vec3, Vec3) -> Vec3
changeBase (Vec3 x y z) (b1, b2, b3) = ((fVec3 x) * b1) + ((fVec3 y) * b2) + ((fVec3 z) * b3)

chooseBase :: Vec3 -> Vec3
chooseBase (Vec3 x y z) = if (x > 0.0) then (Vec3 1.0 0.0 0.0) else (Vec3 0.0 1.0 0.0)

randomHemisphereVec3 :: Vec3 -> Float -> Vec3
randomHemisphereVec3 dir roughness = changeBase (randomZAxisHemisphereVec3 roughness) (b3, b2, b1) where
  b1 = normalize dir
  b2 = normalize (vecMul dir (vecMul dir (chooseBase dir)))
  b3 = normalize (vecMul dir (chooseBase dir))

data Material = Diffuse Vec3 | Mirror Vec3 | Glossy Vec3 Float | Emission Vec3
    deriving Show

red = Vec3 1.0 0.0 0.0
green = Vec3 0.0 1.0 0.0
blue = Vec3 0.0 0.0 1.0
white = Vec3 0.8 0.8 0.8
black = Vec3 0.0 0.0 0.0
purple = Vec3 0.7 0.0 1.0
nullcol = Vec3 0 0 0
bgcolor = Vec3 0.3 0.3 0.3

data Vertex = Vertex{vertPos :: Vec3, vertNml :: Vec3}
    deriving Show

nullVert = (Vertex (Vec3 0.0 0.0 0.0) (Vec3 0.0 0.0 0.0))

data Face = Face{faceV1 :: Vertex, faceV2 :: Vertex, faceV3 :: Vertex, faceMat :: Material}
    deriving Show

nullFace = (Face nullVert nullVert nullVert (Emission purple))

add5 :: Int -> Int
add5 x = x + 5

emissive :: Face -> Bool
emissive (Face _ _ _ (Emission col)) = True
emissive _ = False


getCol :: Face -> Vec3
getCol (Face _ _ _ (Diffuse col)) = col
getCol (Face _ _ _ (Mirror col)) = col
getCol (Face _ _ _ (Glossy col _)) = col
getCol (Face _ _ _ (Emission col)) = col
getCol _ = nullcol

data Ray = Ray{rayPos :: Vec3, rayDir :: Vec3}
    deriving Show

lerp :: Vec3 -> Vec3 -> Float -> Vec3
lerp v1 v2 fac = ((fVec3 (1.0 - fac)) * v1) + ((fVec3 fac) * v2)

normal :: Face -> Vec3
normal (Face (Vertex _ n1) (Vertex _ n2) (Vertex _ n3) _) = (n1+n2+n3)*(fVec3 (1.0/3.0))

flatNormal :: Face -> Vec3
flatNormal (Face (Vertex p1 _) (Vertex p2 _) (Vertex p3 _) _) = normalize (vecMul (p3-p1) (p2-p1))

-- normal :: Face -> Vec3 -> Vec3  Smooth normals... Sometime...

-- TODO: better pattern matching here?
reflectFace :: Face -> Vec3 -> Vec3
reflectFace (Face p1 p2 p3 (Diffuse col)) dir = randomHemisphereVec3 (normal (Face p1 p2 p3 (Diffuse col))) 0.0
reflectFace (Face p1 p2 p3 (Mirror col)) dir = reflect dir (normal (Face p1 p2 p3 (Mirror col)))
reflectFace (Face p1 p2 p3 (Glossy col roughness)) dir = ((fVec3 (1.0 - roughness)) * (reflect dir (normal (Face p1 p2 p3 (Glossy col roughness))))) - 
    ((fVec3 roughness) * (randomHemisphereVec3 (normal (Face p1 p2 p3 (Glossy col roughness))) 0.0))

firstP :: Face -> Vec3
firstP (Face (Vertex p _) _ _ _) = p


intersectFace :: Face -> Ray -> Vec3
--intersectFace face (Ray pos dir) = (lerp pos (pos+dir) ((scalMul n (firstP face)) / (scalMul n dir)))
--intersectFace face (Ray pos dir) = (lerp pos (pos+dir) ((-((scalMul n pos) + (scalMul n (firstP face)))) / (scalMul n dir)))
--    where n = flatNormal face
intersectFace face (Ray l0 l) = l0 + ((fVec3 (((scalMul ((firstP face)-l0) n) / (scalMul l n)))) * l) where
    n = (normal face)

bounded :: Float -> Float -> Float -> Bool
bounded min max x = (min <= x) && (max >= x)

eps = 0.0001
epsEq :: Float -> Float -> Bool
epsEq a b = (abs (a-b)) < eps

area :: Vec3 -> Vec3 -> Vec3 -> Float
area v1 v2 v3 = vlen (vecMul (v2-v1) (v3-v1))

inside :: Vec3 -> Face -> Bool
inside v (Face (Vertex a _) (Vertex b _) (Vertex c _) _) = epsEq (area a b c) ((area a b v) + (area a c v) + (area b c v))

-- ! Barycentric coordinate version, possibly not working
-- inside :: Vec3 -> Face -> Bool
-- inside v (Face (Vertex a _) (Vertex b _) (Vertex c _) _) = (bounded 0 1 alpha) && (bounded 0 1 beta) && (bounded 0 1 gamma) && (epsEq 1 ((alpha + beta) + gamma)) where 
--     area = (vlen (vecMul (b-a) (c-a))) / 2.0
--     alpha = (vlen (vecMul (b-v) (c-v))) / (2.0 * area)
--     beta = (vlen (vecMul (c-v) (a-v))) / (2.0 * area)
--     gamma = 1 - (alpha + beta)

intersectAllH :: [Face] -> Ray -> [(Face, Vec3)] -> [(Face, Vec3)]
intersectAllH [] _ res = res
intersectAllH (fc:faces) ray res = intersectAllH faces ray (if (inside itp fc) then (fc, itp):res else res) where itp=intersectFace fc ray

intersectAll :: [Face] -> Ray -> [(Face, Vec3)]
intersectAll faces ray = intersectAllH faces ray []

closestH :: Vec3 -> [(Face, Vec3)] -> (Face, Vec3) -> Float -> (Face, Vec3)
closestH centre [] res len = res
closestH centre (tup:vecs) old len = if(newlen < len) then (closestH centre vecs tup newlen) else (closestH centre vecs old len) where newlen = vlen ((snd tup) - centre)

closest :: Vec3 -> [(Face, Vec3)] -> (Face, Vec3)
closest centre [] = (nullFace, centre)
closest centre (tup:vecs) = closestH centre vecs tup (vlen ((snd tup) - centre))

trace :: [Face] -> Vec3 -> Ray -> Int -> Vec3
trace faces exposure ray 3 = nullcol
trace faces exposure (Ray pos dir) depth = 
    if(null hitpoints) 
        then (exposure * bgcolor) 
        else if(emissive face) 
            then (exposure * (getCol face)) 
            else (trace faces (exposure * (getCol face)) (Ray hit (reflectFace face dir)) (depth+1)) 
                where 
                    hitpoints = intersectAll faces (Ray pos dir)
                    res = (closest pos hitpoints)
                    face = (fst res)
                    hit = (snd res)


rotateZ :: Float -> Vec3 -> Vec3
rotateZ angle (Vec3 a b c) = (Vec3 
    ((a * (cos angle)) - (b * (sin angle)))
    ((a * (sin angle)) + (b * (cos angle)))
    c
    ) 

rotateY :: Float -> Vec3 -> Vec3
rotateY angle (Vec3 a b c) = (Vec3
    ((a * (cos angle)) + (c * (sin angle)))
    b
    ((c * (cos angle)) - (a * (sin angle)))
    )

rotateX :: Float -> Vec3 -> Vec3
rotateX angle (Vec3 a b c) = (Vec3
        a
        ((b * (cos angle)) - (c * (sin angle)))
        ((c * (cos angle)) + (b * (sin angle)))
        )

remap :: (Float, Float) -> (Float, Float) -> Float -> Float
remap (oldmin, oldmax) (newmin, newmax) val = ((1.0-fac)*newmin)+(fac*newmax) where fac = (val - oldmin) / (oldmax - oldmin)


getCamRay :: (Float, Float, Float) -> (Float, Float) -> Ray -> Ray
getCamRay (width, height, fov) (widthi, heighti) (Ray pos dir) = (Ray pos rotated) where
    xang = remap (0.0, width) (-fov, fov) widthi
    yang = remap (0.0, height) ((-(fov * (width / height))), (fov * (width / height))) heighti
    rotated = (rotateY yang (rotateX xang dir))

traceManyH :: [Face] -> (Float, Float, Float) -> Ray -> (Float, Float) -> [Vec3] -> [Vec3]
traceManyH faces (width, height, fov) cam (wi, hi) resPixels = 
    if(hi >= height) then resPixels
        else if(wi >= width) then traceManyH faces (width, height, fov) cam (0.1, hi+1.0) resPixels
            else traceManyH faces (width, height, fov) cam (wi+1, hi) ((trace faces (fVec3 1.0) (getCamRay (width, height, fov) (wi, hi) cam) 0):resPixels)

traceMany :: [Face] -> (Float, Float, Float) -> Ray -> [Vec3]
traceMany faces screendata cam = traceManyH faces screendata cam (0.1, 0.1) []


to_material :: String -> Material
to_material "Red" = (Diffuse red)
to_material "Green" = (Diffuse green)
to_material "Blue" = (Diffuse blue)
to_material "White" = (Diffuse white)
to_material "EWhite" = (Emission ((fVec3 10.0)*white))
to_material str = (Emission purple)

strListToVec3 :: [String] -> Vec3
strListToVec3 [a, b, c] = Vec3 (read a :: Float) (read b :: Float) (read c :: Float)
strListToVec3 _ = Vec3 0.0 0.0 0.0

stringToVec3 :: String -> Vec3
stringToVec3 str = strListToVec3 (words str)

isCharNum :: Char -> Bool
isCharNum c = and [(ival > (fromEnum '9')), (ival < (fromEnum '0'))] where ival = (fromEnum c)

charToInt :: Char -> Int
charToInt c = if (isCharNum c) then 0 else ival - (fromEnum '0') where ival = (fromEnum c)

-- Reversed result due to being iterative!
separateBySlashesH :: String -> [String] -> [String]
separateBySlashesH [] ss = ss
separateBySlashesH ('/':rest) ("":ss) = separateBySlashesH rest ("":ss)
separateBySlashesH ('/':rest) ss = separateBySlashesH rest ("":ss)
separateBySlashesH (char:rest) ss = separateBySlashesH rest ((char:(head ss)):(tail ss))

separateBySlashes :: String -> [String]
separateBySlashes str = if (and [(not (null sep)), (null (head sep))]) then (tail sep) else sep where sep = (reverse (map reverse (separateBySlashesH str [""])))

rInt :: String -> Int
rInt = read

stringFaceInts :: String -> [Int]
stringFaceInts str = map (rInt) (concat (map separateBySlashes (words str)))

-- """ DRY """ made the code 2x as big with minimal readability improvements and possibly a performance IMPACT
data OBJLine = VertCoords Vec3 | VertNormal Vec3 | FaceIds [Int] | NewMaterial Material | Unknown
    deriving Show

stringToOBJLine :: String -> OBJLine
stringToOBJLine (stripPrefix "v " -> Just rest) = VertCoords (stringToVec3 rest)
stringToOBJLine (stripPrefix "usemtl " -> Just rest) = NewMaterial (to_material rest)
stringToOBJLine (stripPrefix "vn " -> Just rest) = VertNormal (stringToVec3 rest)
stringToOBJLine (stripPrefix "f " -> Just rest) = FaceIds (stringFaceInts rest)
stringToOBJLine _ = Unknown

-- ! Put dummy objects in vertex and vertex normal arrays to offset their indexing by 1 for obj format dumb indexing
loadObjAcc :: ([Vec3], [Vec3], [Face], Material) -> OBJLine -> ([Vec3], [Vec3], [Face], Material)
loadObjAcc (vs, vns, fs, mat) (NewMaterial nmat) = (vs, vns, fs, nmat)
loadObjAcc (vs, vns, fs, mat) (VertCoords v) = (vs ++ [v], vns, fs, mat)
loadObjAcc (vs, vns, fs, mat) (VertNormal vn) = (vs, vns ++ [vn], fs, mat)
loadObjAcc (vs, vns, fs, mat) (FaceIds ids) = (vs, vns, fs ++
    [(Face 
        (Vertex (vs !! (ids !! 0)) (vns !! (ids !! 1))) 
        (Vertex (vs !! (ids !! 2)) (vns !! (ids !! 3))) 
        (Vertex (vs !! (ids !! 4)) (vns !! (ids !! 5)))
        mat
        )], mat)
loadObjAcc tup (Unknown) = tup

dummyLoadTup = ([(Vec3 (1337.0) (1337.0) (1337.0))], [(Vec3 (-1337.0) (-1337.0) (-1337.0))], [], (Emission purple))

trd :: (a, b, c, d) -> c
trd (_, _, a, _) = a

--loadObjH :: Handle -> ([Vec3], [Vec3], [Face], Material) -> [Face]
--loadObjH hdl currAcc = do {
--    eof <- (hIsEOF hdl);
--    if(eof) then (trd currAcc) else (
--        do {
--            line <- (hGetLine hdl);
--            loadObjH hdl (loadObjAcc currAcc line)})}


--loadObj :: Handle -> [Face]
--loadObj hdl = loadObjH hdl dummyLoadTup

loadObj :: String -> [Face]
loadObj str = (trd (foldl loadObjAcc dummyLoadTup (map stringToOBJLine (lines str))))

test1 :: Integer -> Integer -> Integer
test1 x y = x + y + 1

testnml = normalize ((Vec3 4.89395 2.27164 1.06102) - (Vec3 4.967 2.38159 0.951813))
testFace = (Face (Vertex (Vec3 4.967 2.38159 0.951813) testnml) (Vertex (Vec3 5.97786 1.33133 0.570579) testnml) (Vertex (Vec3 6.13322 1.9736 1.32115) testnml) (Emission purple))
testInPoint = (Vec3 5.48638 2.13084 1.04678) -- This point is must be inside the above triangle
testOutPoint = testInPoint - (Vec3 1.0 0.0 0.0) -- Outside

rayStart = (Vec3 4.66603 0.7049 1.40455)
testRay = (Ray rayStart (normalize ((Vec3 4.85275 1.11379 1.27965)-rayStart)))

-- ! These 2 must be equal!
correctRayItsctRes = (Vec3 5.32717 2.1527 0.96229)
testRayItsctRes = intersectFace testFace testRay

ray2Start = (Vec3 2.86 1.76955 1.75255)
testRay2 = (Ray ray2Start (normalize ((Vec3 3.31157 1.7871 1.62369) - ray2Start)))

-- ! These 2 must also be equal!
correctRay2ItsctRes = (Vec3 5.70516 1.88011 0.940681)
testRay2ItsctRes = intersectFace testFace testRay2

testnml2 = normalize ((Vec3 6.5812 0.948004 2.01006) - (Vec3 6.89339 0.999453 1.88656))
testFace2 = (Face (Vertex (Vec3 6.89339 0.999453 1.88656) testnml2) (Vertex (Vec3 6.72319 (-0.366578) 0.88728) testnml2) (Vertex (Vec3 7.51779 (-0.6984) 2.75762) testnml2) (Emission purple))

ray3Start = (Vec3 6.28387 (-0.557073) 4.55117)
testRay3 = (Ray ray3Start (normalize ((Vec3 6.38274 (-0.477723) 4.25429) - ray3Start)))

-- ! Also equal!
correctRay3ItsctRes = (Vec3 7.11149 0.107107 2.06615)
testRay3ItsctRes = intersectFace testFace2 testRay3


-- !     :anger:   my haskell doesn't want to recognize Word8 or GHC.Word.Word8 as types, but will gladly make functions that return those exact same types

-- ? Should the \0 also be written?
bmpFilename = [(w8 (fromEnum 'B')), (w8 (fromEnum 'M'))]

getExtraBytes :: Int -> Int
getExtraBytes w = mod (4 - (mod (w * 3) 4)) 4
-- Padding for lines so they're all a multiple of 4 bytes long

--mkExtraBytes :: Int -> [Word8]
mkExtraBytes 0 = []
mkExtraBytes 1 = [(w8 0)]
mkExtraBytes 2 = [(w8 0), (w8 0)]
mkExtraBytes 3 = [(w8 0), (w8 0), (w8 0)]
-- mkExtraBytes _ = undefined -- ! <-This should never be called

--Black magic headers
mkHeadersInt :: (Int, Int) -> [Int]
mkHeadersInt (w, h) = (paddedsize + 54):0:54:40:w:h:((shift 24 16) + (shift 1 0)):0:paddedsize:0:0:0:0:[] where
    extrabytes = getExtraBytes w
    paddedsize = ((w * 3) + extrabytes) * h

--intToWord8 :: Int -> [Word8]
intToWord8 val = reverse [(w8 (shift val (-24))), (w8 (shift val (-16))), (w8 (shift val (-8))), (w8 val)]

--mkHeaders :: (Int, Int) -> [Word8]
mkHeaders dims = concat (map intToWord8 (mkHeadersInt dims))

--reduce :: Float -> GHC.Word.Word8
reduce num
    | (num >= 1.0) = (w8 255)
    | (num <= 0.0) = (w8 0)
    | otherwise = (w8 (floor (num*255.0)))
    
--Dumb bgr colors because that's what the stackoverflow solution used
--vec3ToW8L :: Vec3 -> [Word8]
vec3ToW8L (Vec3 r g b) = (reduce b):(reduce g):(reduce r):[]
    
--colorsToBytestringH :: (Int, Int) -> [Vec3] -> (Int, Int) -> [Word8] -> [Word8]
colorsToW8LH _ [] _ res = res
colorsToW8LH (w, h) (col:cols) (wi, hi) res
    | (wi == 0) = colorsToW8LH (w, h) (col:cols) (w, hi+1) (res ++ (mkExtraBytes (getExtraBytes w))) --BMPs have padding at the end of each line.
    | otherwise = colorsToW8LH (w, h) cols (wi-1, hi) (res ++ (vec3ToW8L col)) -- And they have their colors writen from bottom to top.
    
--colorsToW8L :: (Int, Int) -> [Vec3] -> [Word8]
colorsToW8L (w, h) (col:cols) = colorsToW8LH (w, h) cols (w-1, 0) (vec3ToW8L col)
    
--makeBMP :: (Int, Int) -> [Vec3] -> [Word8]
makeBMP dims cols = bmpFilename ++ (mkHeaders dims) ++ (colorsToW8L dims cols)
    
    
writeImage :: (Int, Int) -> [Vec3] -> IO Bool
writeImage dimensions pixels = undefined
    
width = 100.0
height = 100.0

main :: IO ()
main = do 
    -- writeFile "output" $ show $ ...
    BL.writeFile "render.bmp" (BL.pack (makeBMP ((floor width), (floor height)) (traceMany (loadObj "mtllib cornell_simple.mtl\no Cube\nv -4.000000 -4.000000 4.000000\nv -4.000000 4.000000 4.000000\nv -4.000000 -4.000000 -4.000000\nv -4.000000 4.000000 -4.000000\nv 4.000000 -4.000000 4.000000\nv 4.000000 4.000000 4.000000\nv 4.000000 -4.000000 -4.000000\nv 4.000000 4.000000 -4.000000\nvn -1.0000 0.0000 0.0000\nvn 1.0000 0.0000 0.0000\nvn 0.0000 0.0000 1.0000\nvn 0.0000 -1.0000 0.0000\nvn 0.0000 1.0000 0.0000\nusemtl Green\ns off\nf 2//1 3//1 1//1\nf 2//1 4//1 3//1\nusemtl Red\nf 8//2 5//2 7//2\nf 8//2 6//2 5//2\nusemtl White\nf 6//3 1//3 5//3\nf 7//4 1//4 3//4\nf 4//5 6//5 8//5\nf 6//3 2//3 1//3\nf 7//4 5//4 1//4\nf 4//5 2//5 6//5\no Cube.001\nv 1.032842 -4.123214 2.313145\nv 1.032842 -2.123214 2.313145\nv -0.381372 -4.123214 0.898931\nv -0.381372 -2.123214 0.898931\nv 2.447055 -4.123214 0.898931\nv 2.447055 -2.123214 0.898931\nv 1.032842 -4.123214 -0.515282\nv 1.032842 -2.123210 -0.515282\nvn -0.7071 0.0000 0.7071\nvn -0.7071 0.0000 -0.7071\nvn 0.7071 0.0000 -0.7071\nvn 0.7071 0.0000 0.7071\nvn 0.0000 -1.0000 0.0000\nvn 0.0000 1.0000 0.0000\nusemtl Blue\ns off\nf 10//6 11//6 9//6\nf 12//7 15//7 11//7\nf 15//8 14//8 13//8\nf 14//9 9//9 13//9\nf 15//10 9//10 11//10\nf 12//11 14//11 16//11\nf 10//6 12//6 11//6\nf 12//7 16//7 15//7\nf 15//8 16//8 14//8\nf 14//9 10//9 9//9\nf 15//10 13//10 9//10\nf 12//11 10//11 14//11\no Cube.002\nv -3.520742 -4.092613 1.154484\nv -3.520742 0.000255 1.154484\nv -2.625176 -4.092613 -0.633800\nv -2.625176 0.000255 -0.633800\nv -1.732458 -4.092613 2.050050\nv -1.732458 0.000255 2.050050\nv -0.836891 -4.092613 0.261766\nv -0.836891 0.000255 0.261766\nvn -0.8941 0.0000 -0.4478\nvn 0.4478 0.0000 -0.8941\nvn 0.8941 0.0000 0.4478\nvn -0.4478 0.0000 0.8941\nvn 0.0000 -1.0000 0.0000\nvn 0.0000 1.0000 0.0000\nusemtl White\ns off\nf 18//12 19//12 17//12\nf 20//13 23//13 19//13\nf 24//14 21//14 23//14\nf 22//15 17//15 21//15\nf 23//16 17//16 19//16\nf 20//17 22//17 24//17\nf 18//12 20//12 19//12\nf 20//13 24//13 23//13\nf 24//14 22//14 21//14\nf 22//15 18//15 17//15\nf 23//16 21//16 17//16\nf 20//17 18//17 22//17\no Plane\nv -1.000000 3.900000 1.000000\nv 1.000000 3.900000 1.000000\nv -1.000000 3.900000 -1.000000\nv 1.000000 3.900000 -1.000000\nvn 0.0000 1.0000 0.0000\nusemtl EWhite\ns off\nf 26//18 27//18 25//18\nf 26//18 28//18 27//18") (width, height, 0.5) (Ray (Vec3 0.0 0.0 (-15.0)) (Vec3 (0.0) (0.0) (-1.0)))) ))
        
        
-- Path: "D:\\Users\\vikto_000\\Documents\\gh-repos\\fp-pract-tasks-1920-Viktorsmg\\RT\\hask_rt.hs"