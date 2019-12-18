{-# LANGUAGE ViewPatterns #-}

import System.Random
import Data.List
import Data.Char
import System.IO  
import Numeric
import Data.Bits
import Data.Bits.Extras
import GHC.Word
import Control.Parallel
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

dist :: Vec3 -> Vec3 -> Float
dist v1 v2 = vlen (v2-v1)

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




randItvl :: Float -> Float -> Float
randItvl min max = 0.5 * (min + max)

randItvlSeeded :: Float -> Float -> Float -> Float
randItvlSeeded min max seed = remap ((-1), 1) (min, max) (nextRand3 seed)

-- Polar coords
-- z>0 always
randomZAxisHemisphereVec3 :: Float -> Vec3
randomZAxisHemisphereVec3 roughness = let
  z = randItvl (cos (roughness * pi)) 1
  angle = randItvl (-pi) (pi)
  r = sqrt (1.0 - (z * z))
  in Vec3 (r * (sin angle)) (r * (cos angle)) z

randomZAxisHemisphereVec3Seeded :: Float -> Float -> Vec3
randomZAxisHemisphereVec3Seeded roughness seed = let
    z = randItvlSeeded (cos (roughness * pi)) 1 (nextRand3 seed)
    angle = randItvlSeeded (-pi) (pi) (nextRand1 (nextRand3 seed))
    r = sqrt (1.0 - (z * z))
    in Vec3 (r * (sin angle)) (r * (cos angle)) z

changeBase :: Vec3 -> (Vec3, Vec3, Vec3) -> Vec3
changeBase (Vec3 x y z) (b1, b2, b3) = ((fVec3 x) * b1) + ((fVec3 y) * b2) + ((fVec3 z) * b3)

chooseBase :: Vec3 -> Vec3
chooseBase (Vec3 x y z) = if ((abs (x)) < 0.5) then (Vec3 1.0 0.0 0.0) else (Vec3 0.0 1.0 0.0)

randomHemisphereVec3 :: Vec3 -> Float -> Vec3
randomHemisphereVec3 dir roughness = changeBase (randomZAxisHemisphereVec3 roughness) (b3, b2, b1) where
  b1 = normalize dir
  b2 = normalize (vecMul dir (vecMul dir (chooseBase dir)))
  b3 = normalize (vecMul dir (chooseBase dir))

randomHemisphereVec3Seeded :: Vec3 -> Float -> Float -> Vec3
randomHemisphereVec3Seeded dir roughness seed = changeBase (randomZAxisHemisphereVec3Seeded roughness seed) (b3, b2, b1) where
    b1 = normalize dir
    b2 = normalize (vecMul dir (vecMul dir (chooseBase dir)))
    b3 = normalize (vecMul dir (chooseBase dir))

data Material = Diffuse Vec3 | Mirror Vec3 | Glossy Vec3 Float | Emission Vec3
    deriving Show

red = Vec3 1.0 0.1 0.1
green = Vec3 0.1 1.0 0.1
blue = Vec3 0.1 0.1 1.1
white = Vec3 0.8 0.8 0.8
black = Vec3 0.05 0.05 0.05
purple = Vec3 0.7 0.1 1.0
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
reflectFace (Face p1 p2 p3 (Diffuse col)) dir = randomHemisphereVec3 (normal (Face p1 p2 p3 (Diffuse col))) 1.0
reflectFace (Face p1 p2 p3 (Mirror col)) dir = reflect dir (normal (Face p1 p2 p3 (Mirror col)))
reflectFace (Face p1 p2 p3 (Glossy col roughness)) dir = ((fVec3 (1.0 - roughness)) * (reflect dir (normal (Face p1 p2 p3 (Glossy col roughness))))) - 
    ((fVec3 roughness) * (randomHemisphereVec3 (normal (Face p1 p2 p3 (Glossy col roughness))) 0.0))

reflectFaceSeeded :: Face -> Vec3 -> Float -> Vec3
reflectFaceSeeded (Face p1 p2 p3 (Diffuse col)) dir seed = randomHemisphereVec3Seeded (normal (Face p1 p2 p3 (Diffuse col))) 1.0 seed
reflectFaceSeeded face dir seed = reflectFace face dir

firstP :: Face -> Vec3
firstP (Face (Vertex p _) _ _ _) = p


intersectFace :: Face -> Ray -> Vec3
--intersectFace face (Ray pos dir) = (lerp pos (pos+dir) ((scalMul n (firstP face)) / (scalMul n dir)))
--intersectFace face (Ray pos dir) = (lerp pos (pos+dir) ((-((scalMul n pos) + (scalMul n (firstP face)))) / (scalMul n dir)))
--    where n = flatNormal face
intersectFace face (Ray l0 l) = l0 + ((fVec3 (if(d<0.0) then d else (1000.0*d))) * l) where
    n = (normal face)
    d = (((scalMul ((firstP face)-l0) n) / (scalMul l n)))

bounded :: Float -> Float -> Float -> Bool
bounded min max x = (min <= x) && (max >= x)

eps = 0.0001
epsEq :: Float -> Float -> Bool
epsEq a b = (abs (a-b)) < eps

area :: Vec3 -> Vec3 -> Vec3 -> Float
area v1 v2 v3 = vlen (vecMul (v2-v1) (v3-v1))

inside :: Vec3 -> Face -> Bool
inside v (Face (Vertex a _) (Vertex b _) (Vertex c _) _) = epsEq (area a b c) ((area a b v) + (area a c v) + (area b c v))

--Curry this on its ray
foldIntersect :: Ray -> [(Face, Vec3)] -> Face
foldIntersect ray acc face =  if (and [(inside itp face), (dist itp (rayPos ray)) > 0.03]) then (face, itp):acc else acc where 
    itp = intersectFace face ray

intersectAll :: [Face] -> Ray -> [(Face, Vec3)]
intersectAll faces ray = foldl' (foldIntersect ray) [] faces

closestH :: Vec3 -> [(Face, Vec3)] -> (Face, Vec3) -> Float -> (Face, Vec3)
closestH centre [] res len = res
closestH centre (tup:vecs) old len = if(newlen < len) then (closestH centre vecs tup newlen) else (closestH centre vecs old len) where newlen = vlen ((snd tup) - centre)

closest :: Vec3 -> [(Face, Vec3)] -> (Face, Vec3)
closest centre [] = (nullFace, centre)
closest centre (tup:vecs) = closestH centre vecs tup (vlen ((snd tup) - centre))

trace :: [Face] -> Vec3 -> Int -> Ray -> Vec3
trace faces exposure 8 ray = nullcol
trace faces exposure depth (Ray pos dir) = 
    if(null hitpoints) 
        then (exposure * bgcolor) 
        else if(emissive face) 
            then (exposure * (getCol face)) 
            else (trace faces (exposure * (getCol face)) (depth+1) (Ray hit (reflectFace face dir))) 
                where 
                    hitpoints = intersectAll faces (Ray pos dir)
                    res = (closest pos hitpoints)
                    face = (fst res)
                    hit = (snd res)

traceSeeded :: [Face] -> Vec3 -> Int -> Ray -> Float -> Vec3
traceSeeded _ _ 8 _ _ = nullcol
traceSeeded faces exposure depth (Ray pos dir) seed = 
    if(null hitpoints) 
        then (exposure * bgcolor) 
        else if(emissive face) 
            then (exposure * (getCol face)) 
            else (traceSeeded faces (exposure * (getCol face)) (depth+1) (Ray hit (reflectFaceSeeded face dir (nextRand3 seed))) (nextRand1 (nextRand2 seed))) 
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
remap (oldmin, oldmax) (newmin, newmax) val = ((1.0-fac)*newmin)+(fac*newmax) where 
    fac = (val - oldmin) / (oldmax - oldmin)

-- TODO: Rewrite ray-generating functions to use oriented camera
data Camera = Camera {looker :: Ray, camUp :: Vec3, camWidth :: Float, camHeight :: Float}

getCamRayPolar :: (Float, Float, Float) -> (Float, Float) -> Ray -> Ray
getCamRayPolar (width, height, fov) (widthi, heighti) (Ray pos dir) = (Ray pos rotated) where
    ang1 = remap (0.0, width) (-fov, fov) widthi
    ang2 = remap (0.0, height) ((-(fov * (width / height))), (fov * (width / height))) heighti
    rotated = (rotateX ang2 (rotateY ang1 dir))

getCamRayLerp :: (Float, Float, Float) -> (Float, Float) -> Ray -> Ray
getCamRayLerp (w, h, fov) (wi, hi) (Ray pos dir) = (Ray pos (lerp (lerp baseCorner sideCorner1 (remap (0, w) (0, 1) wi)) sideCorner2 (remap (0, h) (0, 1) hi))) where
    hfov = (fov * (w / h))
    baseCorner = (rotateY hfov (rotateX fov dir))
    sideCorner1 = (rotateY (-hfov) (rotateX fov dir))
    sideCorner2 = (rotateY hfov (rotateX (-fov) dir))

getCamRayUpDir :: (Float, Float, Float) -> (Float, Float) -> Ray -> Ray
getCamRayUpDir (w, h, fov) (wi, hi) (Ray pos dir) = (Ray pos (normalize (corner - ((fVec3 (remap (0, w-1) (0, 1) wi)) * sideVec) + ((fVec3 (remap (0, h-1) (0, 1) hi)) * upVec) ))) where
    hfov = (fov * (w / h))
    upVec = (Vec3 0.0 1.0 0.0) * (fVec3 (1 / (remap (0, 180) (1, 0) fov)))
    sideVec = (normalize (vecMul dir upVec)) * (fVec3 (1 / (remap (0, 180) (1, 0) hfov)))
    corner = (dir - (upVec * (fVec3 0.5))) + (sideVec * (fVec3 0.5))

getCamRaysH :: ((Float, Float, Float) -> (Float, Float) -> Ray -> Ray) -> (Float, Float, Float) -> Ray -> (Float, Float) -> [Ray] -> [Ray]
getCamRaysH getter (width, height, fov) cam (wi, hi) res
    | (hi >= height) = res
    | (wi >= width) = getCamRaysH getter (width, height, fov) cam (0, hi+1) res
    | otherwise = getCamRaysH getter (width, height, fov) cam (wi+1, hi) ((getter (width, height, fov) (wi, hi) cam ):res)

getCamRaysPolar :: (Float, Float, Float) -> Ray -> [Ray]
getCamRaysPolar screendata cam = reverse $ getCamRaysH getCamRayPolar screendata cam (0.1, 0.1) []

getCamRays :: (Float, Float, Float) -> Ray -> [Ray]
getCamRays screendata cam = reverse $ getCamRaysH getCamRayUpDir screendata cam (0.1, 0.1) []

traceMany :: [Face] -> (Float, Float, Float) -> Ray -> [Vec3]
traceMany faces screendata cam = map (trace faces (Vec3 1.0 1.0 1.0) 0) (getCamRays screendata cam)

traceManySeeded :: [Face] -> (Float, Float, Float) -> Ray -> StdGen -> [Vec3]
traceManySeeded faces (w, h, fov) cam gen = map2 (traceSeeded faces (Vec3 1.0 1.0 1.0) 0) (getCamRays (w, h, fov) cam) (makeRandomArray gen (w*h))

--[Face] -> Vec3 -> Int -> Ray -> Float -> Vec3
traceSeededSPPH :: [Face] -> Vec3 -> Int -> Ray -> Float -> Vec3 -> Vec3
traceSeededSPPH faces exposure 0 ray seed res = res
traceSeededSPPH faces exposure times ray seed res = (traceSeededSPPH faces exposure (times - 1) ray (nextRand2 seed) (res+(traceSeeded faces exposure 0 ray seed)))

traceSeededSPP :: [Face] -> Vec3 -> Int -> Ray -> Float -> Vec3
traceSeededSPP faces exposure times ray seed = (traceSeededSPPH faces exposure times ray seed (Vec3 0.0 0.0 0.0)) * (fVec3 (1.0 / (fromIntegral times)))

traceManySeededSPP :: [Face] -> (Float, Float, Float) -> Ray -> Int -> StdGen -> [Vec3]
traceManySeededSPP faces (w, h, fov) cam spp gen = zipWith (traceSeededSPP faces (Vec3 1.0 1.0 1.0) spp) (getCamRays (w, h, fov) cam) (makeRandomArray gen (w*h))

traceManySeededSPPThreaded :: Int -> [Face] -> (Float, Float, Float) -> Ray -> Int -> StdGen -> [Vec3]
traceManySeededSPPThreaded splitsize faces (w, h, fov) cam spp gen = parmap2 splitsize (traceSeededSPP faces (Vec3 1.0 1.0 1.0) spp) (getCamRays (w, h, fov) cam) (makeRandomArray gen (w*h))


par2 :: (a -> b -> c) -> a -> b -> c
par2 f x y = x `par` y `par` f x y

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f [] _ = []
map2 f _ [] = []
map2 f (a:as) (b:bs) = (f a b):(map2 f as bs)


splitListH :: Int -> [a] -> Int -> [[a]] -> [[a]]
splitListH chunksize [] chunki res = res
splitListH chunksize (x:xs) chunki ([]:ress) = splitListH chunksize xs (chunki+1) ([x]:ress)
splitListH chunksize (x:xs) chunki ((re:res):ress)
    | (chunki == chunksize) = splitListH chunksize (x:xs) 0 ([]:(re:res):ress)
    | otherwise = splitListH chunksize xs (chunki+1) ((x:(re:res)):ress)

-- Splits a list into (firstparam) pieces of roughly the same size.
-- Last piece has the off-size.
splitListEq :: Int -> [a] -> [[a]]
splitListEq chunkcount [] = [[]]
splitListEq chunkcount xs = splitListH ((div (length xs) chunkcount)+(if((mod (length xs) chunkcount) == 0) then 0 else 1)) xs 0 [[]]

splitList :: Int -> [a] -> [[a]]
splitList _ [] = []
splitList n xs = take n xs : splitList n (drop n xs)

parmap2Presplit :: (a -> b -> c) ->[[a]] -> [[b]] -> [[c]]
parmap2Presplit f [] _ = []
parmap2Presplit f _ [] = []
parmap2Presplit f (xs:xss) (ys:yss) = par2 (:) (zipWith f xs ys) (parmap2Presplit f xss yss)

parmap2 :: Int -> (a -> b -> c) -> [a] -> [b] -> [c]
parmap2 splitsize f xs ys = reverse (concat (parmap2Presplit f (splitList splitsize xs) (splitList splitsize ys)))


fract :: Float -> Float
fract x 
    | (x > 0) = x - (fromIntegral (floor x))
    | otherwise = x - (fromIntegral (ceiling x))

-- TODO: BETTER RAND FUNCTIONS

nextRand1 :: Float -> Float
nextRand1 f = cos(fract ( tan(sin( (f / 43.1239) * (f - 9.9) ) * 43758.5453)));

nextRand2 :: Float -> Float
nextRand2 f = cos(fract ( tan(cos( (f / 78.2371) * (f + 31.31) ) * 93172.6584)));

nextRand3 :: Float -> Float
nextRand3 f = sin ( fract ( tan (sin( (f - 4134.7546) / (f * 43.31) ) * 15486.314 )));

nextRand4 :: Float -> Float
nextRand4 f = sin (fract ( tan (cos( (f / 58.7652) * (f + 534.876) ) * 8275.52444)));

makeRandomArray :: StdGen -> Float -> [Float]
makeRandomArray gen length 
    | (length < 0.1) = []
    | otherwise = take (round length) $ (randomRs (-1.0, 1.0) gen)




----------------------------------------------------------------------
--                                OBJ
----------------------------------------------------------------------

-- This is needed because OBJ's default material description type suckssssssss and isn't designed for PBR
-- Just leave this, it's for convenience
toMaterial :: String -> Material
toMaterial "Red" = Diffuse red
toMaterial "Green" = Diffuse green
toMaterial "Blue" = Diffuse blue
toMaterial "White" = Diffuse white
toMaterial "EWhite" = Emission ((fVec3 10.0)*white)
toMaterial "Mirror" = Mirror white
toMaterial str = Emission purple

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
stringToOBJLine (stripPrefix "usemtl " -> Just rest) = NewMaterial (toMaterial rest)
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

loadObj :: String -> [Face]
loadObj str = (trd (foldl loadObjAcc dummyLoadTup (map stringToOBJLine (lines str))))


----------------------------------------------------------------------
--                                BMP
----------------------------------------------------------------------


bmpFilename = [(w8 (fromEnum 'B')), (w8 (fromEnum 'M'))]

getExtraBytes :: Int -> Int
getExtraBytes w = mod (4 - (mod (w * 3) 4)) 4
-- Padding for lines so they're all a multiple of 4 bytes long

mkExtraBytesIns 0 res = res
mkExtraBytesIns 1 res = (w8 0):res
mkExtraBytesIns 2 res = (w8 0):(w8 0):res
mkExtraBytesIns 3 res = (w8 0):(w8 0):(w8 0):res

--Black magic headers
mkHeadersInt :: (Int, Int) -> [Int]
mkHeadersInt (w, h) = (paddedsize + 54):0:54:40:w:h:((shift 24 16) + (shift 1 0)):0:paddedsize:0:0:0:0:[] where
    extrabytes = getExtraBytes w
    paddedsize = ((w * 3) + extrabytes) * h

intToWord8 :: Int -> [Word8]
intToWord8 val = reverse [(w8 (shift val (-24))), (w8 (shift val (-16))), (w8 (shift val (-8))), (w8 val)]

mkHeaders :: (Int, Int) -> [Word8]
mkHeaders dims = concat (map intToWord8 (mkHeadersInt dims))

reduce :: Float -> Word8
reduce num
    | (num >= 1.0) = (w8 255)
    | (num <= 0.0) = (w8 0)
    | otherwise = (w8 (floor (num*255.0)))
    
--Dumb bgr colors because that's what the stackoverflow solution used
vec3ToW8L :: Vec3 -> [Word8]
vec3ToW8L (Vec3 r g b) = [(reduce b), (reduce g), (reduce r)]

addVec3toW8L (Vec3 r g b) res = (reduce r):(reduce g):(reduce b):res

colorsToW8LH :: (Int, Int) -> [Vec3] -> (Int, Int) -> [Word8] -> [Word8]
colorsToW8LH _ [] _ res = res
colorsToW8LH (w, h) (col:cols) (wi, hi) res
    | (wi == 0) = colorsToW8LH (w, h) (col:cols) (w, hi+1) (mkExtraBytesIns (getExtraBytes w) res) --BMPs have padding at the end of each line.
    | otherwise = colorsToW8LH (w, h) cols (wi-1, hi) (addVec3toW8L col res) -- And they have their colors writen from bottom to top.
    
colorsToW8L :: (Int, Int) -> [Vec3] -> [Word8]
colorsToW8L (w, h) (col:cols) = colorsToW8LH (w, h) cols (w-1, 0) (vec3ToW8L col)
    
makeBMP :: (Int, Int) -> [Vec3] -> [Word8]
makeBMP dims cols = bmpFilename ++ (mkHeaders dims) ++ (reverse (colorsToW8L dims cols))

degToRad :: Float -> Float
degToRad x = (x/180.0)*pi
    
------------------------------------
--
--    Change the values below, but not their types, to play around with the camera

-- These 2 are resolutions, they might *say* float, but they shouldn't have anything after the point.
width :: Float
width = 1024.0

height :: Float
height = 1024.0

-- This one is more true to its float type
fov :: Float
fov = 45.0                                 -- 39.5978 to fit the box almost exactly to the camera, use this to test for fixing the distortion?

campos :: Vec3
campos = Vec3 (0.0) (0.0) (-15.0)

-- This will be normalized for you, you pleb
camdir :: Vec3
camdir = Vec3 (0.0) (0.0) (-1.0)

samples = 1

--
--  End of camera variables
--

faces_cornell = (loadObj "mtllib cornell_simple.mtl\no Cube\nv -4.000000 -4.000000 4.000000\nv -4.000000 4.000000 4.000000\nv -4.000000 -4.000000 -4.000000\nv -4.000000 4.000000 -4.000000\nv 4.000000 -4.000000 4.000000\nv 4.000000 4.000000 4.000000\nv 4.000000 -4.000000 -4.000000\nv 4.000000 4.000000 -4.000000\nvn -1.0000 0.0000 0.0000\nvn 1.0000 0.0000 0.0000\nvn 0.0000 0.0000 1.0000\nvn 0.0000 -1.0000 0.0000\nvn 0.0000 1.0000 0.0000\nusemtl Green\ns off\nf 2//1 3//1 1//1\nf 2//1 4//1 3//1\nusemtl Red\nf 8//2 5//2 7//2\nf 8//2 6//2 5//2\nusemtl White\nf 6//3 1//3 5//3\nf 7//4 1//4 3//4\nf 4//5 6//5 8//5\nf 6//3 2//3 1//3\nf 7//4 5//4 1//4\nf 4//5 2//5 6//5\no Cube.001\nv 1.032842 -4.123214 2.313145\nv 1.032842 -2.123214 2.313145\nv -0.381372 -4.123214 0.898931\nv -0.381372 -2.123214 0.898931\nv 2.447055 -4.123214 0.898931\nv 2.447055 -2.123214 0.898931\nv 1.032842 -4.123214 -0.515282\nv 1.032842 -2.123210 -0.515282\nvn -0.7071 0.0000 0.7071\nvn -0.7071 0.0000 -0.7071\nvn 0.7071 0.0000 -0.7071\nvn 0.7071 0.0000 0.7071\nvn 0.0000 -1.0000 0.0000\nvn 0.0000 1.0000 0.0000\nusemtl Blue\ns off\nf 10//6 11//6 9//6\nf 12//7 15//7 11//7\nf 15//8 14//8 13//8\nf 14//9 9//9 13//9\nf 15//10 9//10 11//10\nf 12//11 14//11 16//11\nf 10//6 12//6 11//6\nf 12//7 16//7 15//7\nf 15//8 16//8 14//8\nf 14//9 10//9 9//9\nf 15//10 13//10 9//10\nf 12//11 10//11 14//11\no Cube.002\nv -3.520742 -4.092613 1.154484\nv -3.520742 0.000255 1.154484\nv -2.625176 -4.092613 -0.633800\nv -2.625176 0.000255 -0.633800\nv -1.732458 -4.092613 2.050050\nv -1.732458 0.000255 2.050050\nv -0.836891 -4.092613 0.261766\nv -0.836891 0.000255 0.261766\nvn -0.8941 0.0000 -0.4478\nvn 0.4478 0.0000 -0.8941\nvn 0.8941 0.0000 0.4478\nvn -0.4478 0.0000 0.8941\nvn 0.0000 -1.0000 0.0000\nvn 0.0000 1.0000 0.0000\nusemtl White\ns off\nf 18//12 19//12 17//12\nf 20//13 23//13 19//13\nf 24//14 21//14 23//14\nf 22//15 17//15 21//15\nf 23//16 17//16 19//16\nf 20//17 22//17 24//17\nf 18//12 20//12 19//12\nf 20//13 24//13 23//13\nf 24//14 22//14 21//14\nf 22//15 18//15 17//15\nf 23//16 21//16 17//16\nf 20//17 18//17 22//17\no Plane\nv -1.000000 3.900000 1.000000\nv 1.000000 3.900000 1.000000\nv -1.000000 3.900000 -1.000000\nv 1.000000 3.900000 -1.000000\nvn 0.0000 1.0000 0.0000\nusemtl EWhite\ns off\nf 26//18 27//18 25//18\nf 26//18 28//18 27//18") 
faces_cornell_mirror = (loadObj "# Blender v2.82 (sub 5) OBJ File: 'cornell.blend'\n# www.blender.org\nmtllib cornell_mirror.mtl\no Cube\nv -4.000000 -4.000000 4.000000\nv -4.000000 4.000000 4.000000\nv -4.000000 -4.000000 -4.000000\nv -4.000000 4.000000 -4.000000\nv 4.000000 -4.000000 4.000000\nv 4.000000 4.000000 4.000000\nv 4.000000 -4.000000 -4.000000\nv 4.000000 4.000000 -4.000000\nvn -1.0000 0.0000 0.0000\nvn 1.0000 0.0000 0.0000\nvn 0.0000 0.0000 1.0000\nvn 0.0000 -1.0000 0.0000\nvn 0.0000 1.0000 0.0000\nusemtl Green\ns off\nf 2//1 3//1 1//1\nf 2//1 4//1 3//1\nusemtl Red\nf 8//2 5//2 7//2\nf 8//2 6//2 5//2\nusemtl White\nf 6//3 1//3 5//3\nf 7//4 1//4 3//4\nf 4//5 6//5 8//5\nf 6//3 2//3 1//3\nf 7//4 5//4 1//4\nf 4//5 2//5 6//5\no Cube.001\nv 1.032842 -4.123214 2.313145\nv 1.032842 -2.123214 2.313145\nv -0.381372 -4.123214 0.898931\nv -0.381372 -2.123214 0.898931\nv 2.447055 -4.123214 0.898931\nv 2.447055 -2.123214 0.898931\nv 1.032842 -4.123214 -0.515282\nv 1.032842 -2.123210 -0.515282\nvn -0.7071 0.0000 0.7071\nvn -0.7071 0.0000 -0.7071\nvn 0.7071 0.0000 -0.7071\nvn 0.7071 0.0000 0.7071\nvn 0.0000 -1.0000 0.0000\nvn 0.0000 1.0000 0.0000\nusemtl Blue\ns off\nf 10//6 11//6 9//6\nf 12//7 15//7 11//7\nf 15//8 14//8 13//8\nf 14//9 9//9 13//9\nf 15//10 9//10 11//10\nf 12//11 14//11 16//11\nf 10//6 12//6 11//6\nf 12//7 16//7 15//7\nf 15//8 16//8 14//8\nf 14//9 10//9 9//9\nf 15//10 13//10 9//10\nf 12//11 10//11 14//11\no Cube.002\nv -3.520742 -4.092613 1.154484\nv -3.520742 0.000255 1.154484\nv -2.625176 -4.092613 -0.633800\nv -2.625176 0.000255 -0.633800\nv -1.732458 -4.092613 2.050050\nv -1.732458 0.000255 2.050050\nv -0.836891 -4.092613 0.261766\nv -0.836891 0.000255 0.261766\nvn -0.8941 0.0000 -0.4478\nvn 0.4478 0.0000 -0.8941\nvn 0.8941 0.0000 0.4478\nvn -0.4478 0.0000 0.8941\nvn 0.0000 -1.0000 0.0000\nvn 0.0000 1.0000 0.0000\nusemtl White\ns off\nf 18//12 19//12 17//12\nf 20//13 23//13 19//13\nf 24//14 21//14 23//14\nf 22//15 17//15 21//15\nf 23//16 17//16 19//16\nf 20//17 22//17 24//17\nf 18//12 20//12 19//12\nf 20//13 24//13 23//13\nf 24//14 22//14 21//14\nf 22//15 18//15 17//15\nf 23//16 21//16 17//16\nf 20//17 18//17 22//17\no Plane\nv -1.000000 3.900000 1.000000\nv 1.000000 3.900000 1.000000\nv -1.000000 3.900000 -1.000000\nv 1.000000 3.900000 -1.000000\nvn 0.0000 1.0000 0.0000\nusemtl EWhite\ns off\nf 26//18 27//18 25//18\nf 26//18 28//18 27//18\no Plane.002\nv 4.227553 0.141747 2.050061\nv 1.022640 0.700466 3.918530\nv 3.204612 3.633348 -0.748627\nv -0.000301 4.192067 1.119842\nvn -0.4696 -0.6318 -0.6166\nusemtl Mirror\ns off\nf 30//19 31//19 29//19\nf 30//19 32//19 31//19\n")


main :: IO ()
main = do 
    -- writeFile "output" $ show $ ...
    gen <- newStdGen
    BL.writeFile "render.bmp" 
        (BL.pack (makeBMP ((floor width), (floor height)) (traceManySeededSPPThreaded 10 faces_cornell_mirror
        (width, height, (degToRad (fov / 2.0))) (Ray campos (normalize camdir)) samples gen) ))
        
        
-- Path: "D:\\Users\\vikto_000\\Documents\\gh-repos\\fp-pract-tasks-1920-Viktorsmg\\RT\\hask_rt.hs"