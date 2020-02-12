{-# LANGUAGE ViewPatterns #-}
module Lib where

-- For a how-to-use/general documentation, please scroll to the bottom of the file.
import System.Random
import Data.List
import Data.Char
import System.IO  
import Numeric
import Data.Bits
import Data.Bits.Extras
import GHC.Word
import Control.Parallel
import Data.List.Split
import qualified Data.ByteString as BL

data Vec3 = Vec3{vecx :: Float, vecy :: Float, vecz :: Float}

instance Show Vec3 where
    show (Vec3 a b c) = "(" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ ")"


instance Num Vec3 where
    (Vec3 x1 y1 z1) + (Vec3 x2 y2 z2) = Vec3 (x1+x2) (y1+y2) (z1+z2)
    (Vec3 x1 y1 z1) - (Vec3 x2 y2 z2) = Vec3 (x1-x2) (y1-y2) (z1-z2)
    (Vec3 x1 y1 z1) * (Vec3 x2 y2 z2) = Vec3 (x1*x2) (y1*y2) (z1*z2)
    abs (Vec3 x1 y1 z1) = Vec3 (abs x1) (abs y1) (abs z1)
    signum (Vec3 x1 y1 z1) = Vec3 (signum x1) (signum y1) (signum z1)
    fromInteger i = Vec3 ifloat ifloat ifloat where
        ifloat = fromInteger i

-- Makes Vec3 out of a single float
fVec3 :: Float -> Vec3
fVec3 x = Vec3 x x x

-- Scalar multiplication, because calling it "something product" doesn't mean anything to me
scalMul :: Vec3 -> Vec3 -> Float
scalMul (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = (x1*x2)+(y1*y2)+(z1*z2)

-- Length of a vector, or the length of the segment (vector) (0,0,0)
vlen :: Vec3 -> Float
vlen v = sqrt (scalMul v v)

-- Distance from one vector to another, or the length of the segmetn (vec1) (vec2)
dist :: Vec3 -> Vec3 -> Float
dist v1 v2 = vlen (v2-v1)

-- Makes it so that a vector's length is 1.
normalize :: Vec3 -> Vec3
normalize (Vec3 x y z) = Vec3 (x/len) (y/len) (z/len)
  where len = vlen (Vec3 x y z)

-- 2x2 determinant
det :: Float -> Float -> Float -> Float -> Float
det a b c d = (a*d) - (c*b)

-- Vector multiplication, again calling it "something product" doesn't mean anything to me
vecMul :: Vec3 -> Vec3 -> Vec3
vecMul (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (det y1 y2 z1 z2) (det z1 z2 x1 x2) (det x1 x2 y1 y2)

-- Makes it so that the given x is returned always bounded in [min, max]
-- = min if x<min, = max if x>max
clamp :: Float -> Float -> Float -> Float
clamp minv maxv x 
    | x<minv = minv
    | x>maxv = maxv
    | otherwise = x 

-- Reflect a ray using an imaginary surface's given normal
reflect :: Vec3 -> Vec3 -> Vec3
reflect ray nml = ray - (fVec3 (2.0 * scalMul ray nml) * nml)



-- I didn't know you COULDN'T just put a random number generating function in the middle of things and have everything work without major changes.
randItvl :: Float -> Float -> Float
randItvl minv maxv = 0.5 * (minv + maxv)

-- Returns a random number in the interval [min, max] given a seed (seed itself is [-1, 1])
randItvlSeeded :: Float -> Float -> Float -> Float
randItvlSeeded minv maxv seed = remap (-1, 1) (minv, maxv) (nextRand3 seed)


-- Polar coords
-- z>0 always
-- Same problem as the unseeded randItvl
randomZAxisHemisphereVec3 :: Float -> Vec3
randomZAxisHemisphereVec3 roughness = let
  z = randItvl (cos (roughness * pi)) 1
  angle = randItvl (-pi) pi
  r = sqrt (1.0 - (z * z))
  in Vec3 (r * sin angle) (r * cos angle) z

-- Gerates a random vector in a z-axis hemisphere, given a section of the hemisphere (1 - full hemisphere, 0 - equivalent to (0,0,1))
-- and a seed. Useful for matte surfaces.
randomZAxisHemisphereVec3Seeded :: Float -> Float -> Vec3
randomZAxisHemisphereVec3Seeded roughness seed = let
    z = randItvlSeeded (cos (roughness * pi)) 1 (nextRand3 seed)
    angle = randItvlSeeded (-pi) pi (nextRand1 (nextRand3 seed))
    r = sqrt (1.0 - (z * z))
    in Vec3 (r * sin angle) (r * cos angle) z

-- Changes the basis of a vector given the new basis (represented through the old one)
changeBase :: Vec3 -> (Vec3, Vec3, Vec3) -> Vec3
changeBase (Vec3 x y z) (b1, b2, b3) = (fVec3 x * b1) + (fVec3 y * b2) + (fVec3 z * b3)

-- Choose a basis for making the random hemisphere oriented.
chooseBase :: Vec3 -> Vec3
chooseBase (Vec3 x _ _) = if abs x < 0.5 then Vec3 1.0 0.0 0.0 else Vec3 0.0 1.0 0.0

-- Generates a random vector in a hemisphere around the given vector, also having the section of the hemisphere as a parameter.
randomHemisphereVec3 :: Vec3 -> Float -> Vec3
randomHemisphereVec3 dir roughness = changeBase (randomZAxisHemisphereVec3 roughness) (b3, b2, normalize dir) where
  b2 = normalize (vecMul dir (vecMul dir (chooseBase dir)))
  b3 = normalize (vecMul dir (chooseBase dir))

-- It needed to be seeded again.
randomHemisphereVec3Seeded :: Vec3 -> Float -> Float -> Vec3
randomHemisphereVec3Seeded dir roughness seed = changeBase (randomZAxisHemisphereVec3Seeded roughness seed) (b3, b2, b1) where
    b1 = normalize dir
    b2 = normalize (vecMul dir (vecMul dir (chooseBase dir)))
    b3 = normalize (vecMul dir (chooseBase dir))

-- Diffuse (Color):
-- A standard "diffuse"/matte material. Like a plank of wood or some drywall.
-- Mirror (Color):
-- A perfectly reflective mirror.
-- Glossy (Color, roughness [0, 1])
-- A smudged metal. Can be a mirror with 0 roughness, or should be diffuse with 1 roughness.
-- Emission (Color):
-- An emmissive surface. A lamp?
-- Note: colors don't have to be bounded, bound them only if you're looking for realism.
-- An exception and use is for emmission, which might need to be a much brighter color (10, 10, 10).
-- But, negative colors are also possible! Though not realistic, I've found that this has some very good artistic applications! 
data Material = Diffuse Vec3 | Mirror Vec3 | Glossy Vec3 Float | Emission Vec3
    deriving Show
-- A few basic colors for common use.
red :: Vec3
red = Vec3 1.0 0.1 0.1
green :: Vec3
green = Vec3 0.1 1.0 0.1
blue :: Vec3
blue = Vec3 0.1 0.1 1.1
white :: Vec3
white = Vec3 0.8 0.8 0.8
black :: Vec3
black = Vec3 0.05 0.05 0.05
purple :: Vec3
purple = Vec3 0.7 0.1 1.0
nullcol :: Vec3
nullcol = Vec3 0 0 0
-- Background color. Change this to change the background. Note - the background also emits light!
bgcolor :: Vec3
bgcolor = Vec3 0.3 0.3 0.3

data Vertex = Vertex{vertPos :: Vec3, vertNml :: Vec3}
    deriving Show

nullVert :: Vertex
nullVert = Vertex (Vec3 0.0 0.0 0.0) (Vec3 0.0 0.0 0.0)

data Face = Face{faceV1 :: Vertex, faceV2 :: Vertex, faceV3 :: Vertex, faceMat :: Material}
    deriving Show

nullFace :: Face
nullFace = Face nullVert nullVert nullVert (Emission purple)

-- Check if a face has an emmissive material (to stop bouncing the ray around)
emissive :: Face -> Bool
emissive (Face _ _ _ (Emission _)) = True
emissive _ = False

-- Get a face's color
getCol :: Face -> Vec3
getCol (Face _ _ _ (Diffuse col)) = col
getCol (Face _ _ _ (Mirror col)) = col
getCol (Face _ _ _ (Glossy col _)) = col
getCol (Face _ _ _ (Emission col)) = col
--getCol _ = nullcol

data Ray = Ray{rayPos :: Vec3, rayDir :: Vec3}
    deriving Show

-- Linear intERPolation
lerp :: Vec3 -> Vec3 -> Float -> Vec3
lerp v1 v2 fac = fVec3 (1.0 - fac) * v1 + fVec3 fac * v2

-- Get the normal of a face, as stored (normals can potentially be changed)
normal :: Face -> Vec3
normal (Face (Vertex _ n1) (Vertex _ n2) (Vertex _ n3) _) = (n1+n2+n3)* fVec3 (1.0/3.0)
-- Generate the "flat" (not smooth) normal of a face
flatNormal :: Face -> Vec3
flatNormal (Face (Vertex p1 _) (Vertex p2 _) (Vertex p3 _) _) = normalize (vecMul (p3-p1) (p2-p1))

-- normal :: Face -> Vec3 -> Vec3  Smooth normals... Sometime...


-- No reflecting emmissive materials!!!
-- Reflects a simple ray on the given face, accounting for its material.
-- Randomness comes into play here.
reflectFace :: Face -> Vec3 -> Vec3
reflectFace face@(Face _ _ _ (Diffuse _)) _ = randomHemisphereVec3 (normal face) 1.0
reflectFace face@(Face _ _ _ (Mirror _)) dir = reflect dir (normal face)
reflectFace face@(Face _ _ _ (Glossy _ roughness)) dir = (fVec3 (1.0 - roughness) * reflect dir (normal face)) - 
    (fVec3 roughness * randomHemisphereVec3 (normal face) 0.0)
-- This isn't exactly correct ^
reflectFace _ _ = error "Trying to reflect off unreflectable material"

reflectFaceSeeded :: Face -> Vec3 -> Float -> Vec3
reflectFaceSeeded face@(Face _ _ _ (Diffuse _)) _ seed = randomHemisphereVec3Seeded (normal face) 1.0 seed
reflectFaceSeeded face@(Face _ _ _ (Glossy _ roughness)) _ seed = randomHemisphereVec3Seeded (normal face) roughness seed
-- Neither is this                                                      ^        but I don't know enough to make it completely correct
reflectFaceSeeded face dir _ = reflect dir (normal face)

firstP :: Face -> Vec3
firstP (Face (Vertex p _) _ _ _) = p

-- It was a pain to find the correct formula.
-- In the end, the correct one is from wikipedia, but needed some tweaking.
intersectFace :: Face -> Ray -> Vec3
--intersectFace face (Ray pos dir) = (lerp pos (pos+dir) ((scalMul n (firstP face)) / (scalMul n dir)))
--intersectFace face (Ray pos dir) = (lerp pos (pos+dir) ((-((scalMul n pos) + (scalMul n (firstP face)))) / (scalMul n dir)))
--    where n = flatNormal face

-- This will intersect faces behind our complex ray as well. So,      \/  the location is put far away so that it gets filtered when looking if an intersection is inside the face (triangle)
intersectFace face (Ray l0 l) = l0 + (fVec3 (if d<0.0 then d else 1000.0*d ) * l) where
    n = normal face
    d = scalMul (firstP face -l0) n / scalMul l n

-- Is x in [min, max]?
bounded :: Float -> Float -> Float -> Bool
bounded minv maxv x = (minv <= x) && (maxv >= x)

eps :: Float
eps = 0.0001
epsEq :: Float -> Float -> Bool
epsEq a b = abs (a-b) < eps

-- Area of a 3d triangle, given its 3 vertices.
area :: Vec3 -> Vec3 -> Vec3 -> Float
area v1 v2 v3 = vlen (vecMul (v2-v1) (v3-v1))
-- Is a point "inside" a 3d triangle? (Or, very very close to its surface - epsEq comes in handy)
inside :: Vec3 -> Face -> Bool
inside v (Face (Vertex a _) (Vertex b _) (Vertex c _) _) = epsEq (area a b c) (area a b v + area a c v + area b c v)

--Curry this on its ray - for intersecting all faces with a fold.
foldIntersect :: Ray -> [(Face, Vec3)] -> Face -> [(Face, Vec3)]
foldIntersect ray acc face =  if inside itp face && dist itp (rayPos ray) > 0.03 then (face, itp):acc else acc where 
    itp = intersectFace face ray
--Intersect all   \/ these faces with the ray.
intersectAll :: [Face] -> Ray -> [(Face, Vec3)]
intersectAll faces ray = foldl' (foldIntersect ray) [] faces

-- Iterative helper for finding the closest intersection point - the physical ray only hits the closest surface 
closestH :: Vec3 -> [(Face, Vec3)] -> (Face, Vec3) -> Float -> (Face, Vec3)
closestH _ [] res _ = res
closestH centre (tup:vecs) old len = if newlen < len then closestH centre vecs tup newlen else closestH centre vecs old len where 
    newlen = vlen (snd tup - centre)

closest :: Vec3 -> [(Face, Vec3)] -> (Face, Vec3)
closest centre [] = (nullFace, centre)
closest centre (tup:vecs) = closestH centre vecs tup (vlen (snd tup - centre))

-- Trace! Give it the faces, the exposure (usually (1.0, 1.0, 1.0)), the starting depth and the ray itself!
-- Max depth is 8, at which point the tracing stops and a black color is returned.
-- You can imagine that at that point the ray has lost too much energy anyways.
-- Make the starting depth lower (negative?) if you want more bounces, but 8 should be enough.

trace :: [Face] -> Vec3 -> Int -> Ray -> Vec3
trace = undefined
--trace faces exposure 8 ray = nullcol
--trace faces exposure depth (Ray pos dir)
--  | null hitpoints = exposure * bgcolor
--  | emissive face = exposure * getCol face
--  | otherwise =
--    trace faces (exposure * getCol face) (depth + 1)
--      (Ray hit (reflectFace face dir))
--  where hitpoints = intersectAll faces (Ray pos dir)
--        res = closest pos hitpoints
--        face = fst res
--        hit = snd res

-- Seeded tracing. Look above.
traceSeeded :: [Face] -> Vec3 -> Int -> Ray -> Float -> Vec3
traceSeeded _ _ 8 _ _ = nullcol
traceSeeded faces exposure depth (Ray pos dir) seed
  | null hitpoints = exposure * bgcolor
  | emissive face = exposure * getCol face
  | otherwise =
    traceSeeded faces (exposure * getCol face) (depth + 1)
      (Ray hit (reflectFaceSeeded face dir (nextRand3 seed)))
      (nextRand1 (nextRand2 seed))
  where hitpoints = intersectAll faces (Ray pos dir)
        res = closest pos hitpoints
        face = fst res
        hit = snd res

-- Rotate vector on the Z axis, radians.
rotateZ :: Float -> Vec3 -> Vec3
rotateZ angle (Vec3 a b c) = Vec3 
    ((a * cos angle) - (b * sin angle))
    ((a * sin angle) + (b * cos angle))
    c
    

-- Rotate vector on the Y axis, radians.
rotateY :: Float -> Vec3 -> Vec3
rotateY angle (Vec3 a b c) = Vec3
    ((a * cos angle) + (c * sin angle))
    b
    ((c * cos angle) - (a * sin angle))
    

-- Rotate vector on the X axis, radians.
rotateX :: Float -> Vec3 -> Vec3
rotateX angle (Vec3 a b c) = Vec3
        a
        ((b * cos angle) - (c * sin angle))
        ((c * cos angle) + (b * sin angle))
        

-- My favorite function, always comes in handy somewhere.
-- Take a range [a, b], a value (supposed to be) in it, and a new range [c, d]
-- Return a value such that both values are gotten through a lerp with the same factor on each range, AKA
-- Both values are in the same position in the range relative to the range's bounds
-- e.g. [0, 1] 0.5 in [5, 7] is 6.0 (still in the middle)
remap :: (Float, Float) -> (Float, Float) -> Float -> Float
remap (oldmin, oldmax) (newmin, newmax) val = ((1.0-fac)*newmin)+(fac*newmax) where 
    fac = (val - oldmin) / (oldmax - oldmin)

-- TODO: Rewrite ray-generating functions to use oriented camera
data Camera = Camera {looker :: Ray, camUp :: Vec3, camWidth :: Float, camHeight :: Float}
-- "Polar" way to get a camera ray. Results in a slightly distorted image.
getCamRayPolar :: (Float, Float, Float) -> (Float, Float) -> Ray -> Ray
getCamRayPolar (width, height, fov) (widthi, heighti) (Ray pos dir) = Ray pos rotated where
    ang1 = remap (0.0, width) (-fov, fov) widthi
    ang2 = remap (0.0, height) (-(fov * (width / height)), fov * (width / height)) heighti
    rotated = rotateX ang2 (rotateY ang1 dir)

-- An attempt to get a camera ray with lerp. Didn't work, the results are quite funky.
getCamRayLerp :: (Float, Float, Float) -> (Float, Float) -> Ray -> Ray
getCamRayLerp (w, h, fov) (wi, hi) (Ray pos dir) = Ray pos (lerp (lerp baseCorner sideCorner1 (remap (0, w) (0, 1) wi)) sideCorner2 (remap (0, h) (0, 1) hi)) where
    hfov = fov * (w / h)
    baseCorner = rotateY hfov (rotateX fov dir)
    sideCorner1 = rotateY (-hfov) (rotateX fov dir)
    sideCorner2 = rotateY hfov (rotateX (-fov) dir)

-- Get camera ray given the camera's upwards direction.
-- This seems like the best way, as it also has no distortion.
getCamRayUpDir :: (Float, Float, Float) -> (Float, Float) -> Ray -> Ray
getCamRayUpDir (w, h, fov) (wi, hi) (Ray pos dir) = Ray pos (normalize (corner - (fVec3 (remap (0, w-1) (0, 1) wi) * sideVec) + (fVec3 (remap (0, h-1) (0, 1) hi) * upVec) )) where
    hfov = fov * (w / h)
    upVec = Vec3 0.0 1.0 0.0 * fVec3 (1 / remap (0, 180) (1, 0) fov)
    sideVec = normalize (vecMul dir upVec) * fVec3 (1 / remap (0, 180) (1, 0) hfov)
    corner = (dir - (upVec * fVec3 0.5)) + (sideVec * fVec3 0.5)

-- Helper for getting many camera rays
getCamRaysH :: ((Float, Float, Float) -> (Float, Float) -> Ray -> Ray) -> (Float, Float, Float) -> Ray -> (Float, Float) -> [Ray] -> [Ray]
getCamRaysH getter (width, height, fov) cam (wi, hi) res
    | hi >= height = res
    | wi >= width = getCamRaysH getter (width, height, fov) cam (0, hi+1) res
    | otherwise = getCamRaysH getter (width, height, fov) cam (wi+1, hi) (getter (width, height, fov) (wi, hi) cam :res)

-- Produce list of camera rays using the polar method.
-- They are ordered in a way that makes them great to form an image out of, aka [line1row1 line1row2 line1row3....   line2row1... lineFinRow1.. lineFinRowfin].
getCamRaysPolar :: (Float, Float, Float) -> Ray -> [Ray]
getCamRaysPolar screendata cam = reverse $ getCamRaysH getCamRayPolar screendata cam (0.1, 0.1) []

-- Using the better upwards-direction ray generator.
getCamRays :: (Float, Float, Float) -> Ray -> [Ray]
getCamRays screendata cam = reverse $ getCamRaysH getCamRayUpDir screendata cam (0.1, 0.1) []

-- Apply tracing on lotsa rays
traceMany :: [Face] -> (Float, Float, Float) -> Ray -> [Vec3]
traceMany faces screendata cam = map (trace faces (Vec3 1.0 1.0 1.0) 0) (getCamRays screendata cam)

-- :<
traceManySeeded :: [Face] -> (Float, Float, Float) -> Ray -> StdGen -> [Vec3]
traceManySeeded faces (w, h, fov) cam gen = map2 (traceSeeded faces (Vec3 1.0 1.0 1.0) 0) (getCamRays (w, h, fov) cam) (makeRandomArray gen (w*h))

--SPP = samples per pixel
traceSeededSPPH :: [Face] -> Vec3 -> Int -> Ray -> Float -> Vec3 -> Vec3
traceSeededSPPH _ _ 0 _ _ res = res
traceSeededSPPH faces exposure times ray seed res = traceSeededSPPH faces exposure (times - 1) ray (nextRand2 seed) (res+ traceSeeded faces exposure 0 ray seed)
-- Trace a seeded ray multiple times with a slight offset, to produce a better "average" result
traceSeededSPP :: [Face] -> Vec3 -> Int -> Ray -> Float -> Vec3
traceSeededSPP faces exposure times ray seed = traceSeededSPPH faces exposure times ray seed (Vec3 0.0 0.0 0.0) * fVec3 (1.0 / fromIntegral times)
-- Trace many rays many times each, seeded
traceManySeededSPP :: [Face] -> (Float, Float, Float) -> Ray -> Int -> StdGen -> [Vec3]
traceManySeededSPP faces (w, h, fov) cam spp gen = zipWith (traceSeededSPP faces (Vec3 1.0 1.0 1.0) spp) (getCamRays (w, h, fov) cam) (makeRandomArray gen (w*h))
-- And with threads!
traceManySeededSPPThreaded :: Int -> [Face] -> (Float, Float, Float) -> Ray -> Int -> StdGen -> [Vec3]
traceManySeededSPPThreaded splitsize faces (w, h, fov) cam spp gen = parmap2 splitsize (traceSeededSPP faces (Vec3 1.0 1.0 1.0) spp) (getCamRays (w, h, fov) cam) (makeRandomArray gen (w*h))


par2 :: (a -> b -> c) -> a -> b -> c
par2 f x y = x `par` y `par` f x y

-- Map for 2-argument function
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _ [] _ = []
map2 _ _ [] = []
map2 f (a:as) (b:bs) = f a b : map2 f as bs

splitListH :: Int -> [a] -> Int -> [[a]] -> [[a]]
splitListH a b c [] = splitListH a b c [[]]
splitListH _ [] _ res = res
splitListH chunksize (x:xs) chunki ([]:ress) = splitListH chunksize xs (chunki+1) ([x]:ress)
splitListH chunksize (x:xs) chunki ((re:res):ress)
    | chunki == chunksize = splitListH chunksize (x:xs) 0 ([]:(re:res):ress)
    | otherwise = splitListH chunksize xs (chunki+1) ((x:(re:res)):ress)


-- Splits a list into (firstparam) pieces of roughly the same size.
-- Last piece has the off-size.
splitListEq :: Int -> [a] -> [[a]]
splitListEq _ [] = [[]]
splitListEq chunkcount xs = splitListH (div (length xs) chunkcount + (if mod (length xs) chunkcount == 0 then 0 else 1)) xs 0 [[]]

-- Split list into (firstparam)-size pieces.
-- Last piece has the off-size.
splitList :: Int -> [a] -> [[a]]
splitList _ [] = []
splitList n xs = take n xs : splitList n (drop n xs)

-- Parallel map2
parmap2 :: Int -> (a -> b -> c) -> [a] -> [b] -> [c]
parmap2 splitsize f xs ys = reverse (concat (parmap2Presplit f (splitList splitsize xs) (splitList splitsize ys)))

-- Parmap2 on an already split-up list
parmap2Presplit :: (a -> b -> c) ->[[a]] -> [[b]] -> [[c]]
parmap2Presplit _ [] _ = []
parmap2Presplit _ _ [] = []
parmap2Presplit f (xs:xss) (ys:yss) = par2 (:) (zipWith f xs ys) (parmap2Presplit f xss yss)

-- Ger fractional part of a number
fract :: Float -> Float
fract val 
    | val > 0 = val - fromIntegral (floor val)
    | otherwise = val - fromIntegral (ceiling val)

-- TODO: BETTER RAND FUNCTIONS

nextRand1 :: Float -> Float
nextRand1 f = cos(fract ( tan(sin( (f / 43.1239) * (f - 9.9) ) * 43758.5453)));

nextRand2 :: Float -> Float
nextRand2 f = cos(fract ( tan(cos( (f / 78.2371) * (f + 31.31) ) * 93172.6584)));

nextRand3 :: Float -> Float
nextRand3 f = sin ( fract ( tan (sin( (f - 4134.7546) / (f * 43.31) ) * 15486.314 )));

nextRand4 :: Float -> Float
nextRand4 f = sin (fract ( tan (cos( (f / 58.7652) * (f + 534.876) ) * 8275.52444)));

-- It'S AcTuAlLy A lIsT
makeRandomArray :: StdGen -> Float -> [Float]
makeRandomArray gen len
    | len < 0.1 = []
    | otherwise = take (round len) $ randomRs (-1.0, 1.0) gen




----------------------------------------------------------------------
--                                OBJ
----------------------------------------------------------------------

-- This is needed because OBJ's default material description type is poor and catered around how old video games do their materials (lacks PBR)
-- And it's too mindboggling for me anyways.
-- So, name a material in the OBJ file one of these names and it will become one of the materials below.
toMaterial :: String -> Material
toMaterial "Red" = Diffuse red
toMaterial "Green" = Diffuse green
toMaterial "Blue" = Diffuse blue
toMaterial "White" = Diffuse white
toMaterial "EWhite" = Emission (fVec3 10.0 * white)
toMaterial "Mirror" = Mirror white
toMaterial _ = Emission purple


-- Should be formatted like this, for example:
-- ["0.123", "0.456", "0.789"]
-- Keep on reading below.
strListToVec3 :: [String] -> Vec3
strListToVec3 [a, b, c] = Vec3 (read a :: Float) (read b :: Float) (read c :: Float)
strListToVec3 _ = Vec3 0.0 0.0 0.0

-- Get such a list by separating values by spaces. (aka the "words")
stringToVec3 :: String -> Vec3
stringToVec3 str = strListToVec3 (words str)

-- Digits are numbers too
isCharNum :: Char -> Bool
isCharNum c = (ival > fromEnum '9') && (ival < fromEnum '0') where ival = fromEnum c

-- The char represents an int, like '3'
charToInt :: Char -> Int
charToInt c = if isCharNum c then 0 else ival - fromEnum '0' where ival = fromEnum c

--Reversed result due to being iterative!
--separateBySlashesH :: String -> [String] -> [String]
--separateBySlashesH [] ss = ss
--separateBySlashesH ('/':rest) ("":ss) = separateBySlashesH rest ("":ss)
--separateBySlashesH ('/':rest) ss = separateBySlashesH rest ("":ss)
--separateBySlashesH sth [] = separateBySlashesH sth [[]]
--separateBySlashesH (char:rest) ss = separateBySlashesH rest ((char : head ss) : tail ss)

-- !Check if this works
--separateBySlashes str =  groupBy (=='/') (filter ('/' `elem`) str)
-- In the obj files, in some lines, values are separated by slashes, e.g.  5/2/1  there are always 2 slashes and values after the first might be missing
separateBySlashes :: String -> [String]
--separateBySlashes = filter ('/' `notElem`) . groupBy (=='/')
separateBySlashes str = filter (/="") (splitOn "/" str)
--separateBySlashes str = if not (null sep) && null (head sep) then tail sep else sep where sep = reverse (map reverse (separateBySlashesH str [""]))

rInt :: String -> Int
rInt str = read str :: Int

-- Get the face numbers that are separated by slashes
stringFaceInts :: String -> [Int]
stringFaceInts str = map rInt (concatMap separateBySlashes (words str))

-- Someone suggested to me that I use special pattern matching and """DRY""" (it only inflated the code size IMO)
data OBJLine = VertCoords Vec3 | VertNormal Vec3 | FaceIds [Int] | NewMaterial Material | Unknown
    deriving Show

stringToOBJLine :: String -> OBJLine
stringToOBJLine (stripPrefix "v " -> Just rest) = VertCoords (stringToVec3 rest)
stringToOBJLine (stripPrefix "usemtl " -> Just rest) = NewMaterial (toMaterial rest)
stringToOBJLine (stripPrefix "vn " -> Just rest) = VertNormal (stringToVec3 rest)
stringToOBJLine (stripPrefix "f " -> Just rest) = FaceIds (stringFaceInts rest)
stringToOBJLine _ = Unknown

-- ! Put dummy objects in vertex and vertex normal arrays to offset their indexing by 1 for obj format dumb indexing
-- For using with a fold, accumulates Vertices, Vertex Normals, Faces and a current material
-- This has a super fat first parameter because of the structure of an obj file - it has direct definitions of vertices,
-- and then references them by their index in the file itself when they're used (it assumes usage of arrays rather than lists, a good assumption)
-- Once the file is completely read, the result we'll need is the array of faces
loadObjAcc :: ([Vec3], [Vec3], [Face], Material) -> OBJLine -> ([Vec3], [Vec3], [Face], Material)
loadObjAcc (vs, vns, fs, _) (NewMaterial nmat) = (vs, vns, fs, nmat)
loadObjAcc (vs, vns, fs, mat) (VertCoords v) = (vs ++ [v], vns, fs, mat)
loadObjAcc (vs, vns, fs, mat) (VertNormal vn) = (vs, vns ++ [vn], fs, mat)
loadObjAcc (vs, vns, fs, mat) (FaceIds ids) = (vs, vns, fs ++
    case ids of 
        (id0:id1:id2:id3:id4:id5:_) -> [Face 
            (Vertex (vs !! id0) (vns !! id1)) -- Ето за това не харесвам хлинт толкова много - набута го тоя хед тук.
            (Vertex (vs !! id2) (vns !! id3))
            (Vertex (vs !! id4) (vns !! id5))
            mat
            ]
        _ -> error "Incorrect number of face indexes; are vertex normals omitted?", mat)
loadObjAcc tup Unknown = tup

dummyLoadTup :: ([Vec3], [Vec3], [Face], Material)
dummyLoadTup = ([Vec3 1337.0 1337.0 1337.0], [Vec3 (-1337.0) (-1337.0) (-1337.0)], [], Emission purple)

trd :: (a, b, c, d) -> c
trd (_, _, a, _) = a

-- Load an obj file, represented as a string.
loadObj :: String -> [Face]
loadObj str = trd (foldl' loadObjAcc dummyLoadTup (map stringToOBJLine (lines str)))


----------------------------------------------------------------------
--                                BMP
----------------------------------------------------------------------

-- Based off of a C++ bmp-drawing function found here:
-- https://en.wikipedia.org/wiki/User:Evercat/Buddhabrot.c

-- This is the file type as stored in the header. Similar to an extension.
-- I don't know why, don't look here for such information.
bmpFiletype :: [Word8]
bmpFiletype = [w8 (fromEnum 'B'), w8 (fromEnum 'M')]

-- Padding for lines so they're all a multiple of 4 bytes long
getExtraBytes :: Int -> Int
getExtraBytes w = mod (4 - mod (w * 3) 4) 4


-- Add padding to a list of wrod8s
mkExtraBytesIns :: Int -> [Word8] -> [Word8]
mkExtraBytesIns 0 res = res
mkExtraBytesIns 1 res = w8 0 : res
mkExtraBytesIns 2 res = w8 0 : w8 0 : res
mkExtraBytesIns 3 res = w8 0 : w8 0 : w8 0 : res
mkExtraBytesIns _ _ = error "Trying to make unexpected amount of padding bytes!"

-- Black magic for making the header
-- If the header has these mystical magical numbers, everything will work juust fiine
-- Knowing what these numbers mean is out of the scope of this project
-- I used ints for simplicity here, converting them to word8 below
mkHeadersInt :: (Int, Int) -> [Int]
mkHeadersInt (w, h) = [paddedsize + 54, 0, 54, 40, w, h, shift 24 16 + shift 1 0, 0, paddedsize, 0, 0, 0, 0] where
    extrabytes = getExtraBytes w
    paddedsize = ((w * 3) + extrabytes) * h

intToWord8 :: Int -> [Word8]
intToWord8 val = reverse [w8 (shift val (-24)), w8 (shift val (-16)), w8 (shift val (-8)), w8 val]

-- The headers, but in word8 type
mkHeaders :: (Int, Int) -> [Word8]
mkHeaders dims = concatMap intToWord8 (mkHeadersInt dims)

-- Reduce a color to one that can be represented in a word8
reduce :: Float -> Word8
reduce num
    | num >= 1.0 = w8 255
    | num <= 0.0 = w8 0
    | otherwise = w8 (floor (num*255.0))
    
--Dumb bgr colors because bmp
vec3ToW8L :: Vec3 -> [Word8]
vec3ToW8L (Vec3 r g b) = [reduce b, reduce g, reduce r]

addVec3toW8L :: Vec3 -> [Word8] -> [Word8]
addVec3toW8L (Vec3 r g b) res = reduce r : reduce g : reduce b : res

colorsToW8LH :: (Int, Int) -> [Vec3] -> (Int, Int) -> [Word8] -> [Word8]
colorsToW8LH _ [] _ res = res
colorsToW8LH (w, h) (col:cols) (wi, hi) res
    | wi == 0 = colorsToW8LH (w, h) (col:cols) (w, hi+1) (mkExtraBytesIns (getExtraBytes w) res) --BMPs have padding at the end of each line.
    | otherwise = colorsToW8LH (w, h) cols (wi-1, hi) (addVec3toW8L col res) -- And they have their colors writen from bottom to top.


-- Make the bmp's color array (doesn't include headers!!!)
colorsToW8L :: (Int, Int) -> [Vec3] -> [Word8]
colorsToW8L (w, h) (col:cols) = colorsToW8LH (w, h) cols (w-1, 0) (vec3ToW8L col)
colorsToW8L _ [] = []

-- Out of an array of colors, and its image dimensions, make an array of word8s that's ready for writing to a file.
makeBMP :: (Int, Int) -> [Vec3] -> [Word8]
makeBMP dims cols = bmpFiletype ++ mkHeaders dims ++ reverse (colorsToW8L dims cols)

degToRad :: Float -> Float
degToRad x = (x/180.0)*pi
    
------------------------------------
--
--    Change the values below, but not their types, to play around with the camera

-- These 2 are resolutions, they might *say* float, but they shouldn't have anything after the point.
widthRender :: Float
widthRender = 1024.0

heightRender :: Float
heightRender = 1024.0

-- This one is more true to its float type
-- Also, is in degrees
fovRender :: Float
fovRender = 45.0                                 -- 39.5978 to fit the box almost exactly to the camera, use this to test for fixing the distortion?

campos :: Vec3
campos = Vec3 0.0 0.0 (-15.0)

-- This will be normalized for you, you pleb
camdir :: Vec3
camdir = Vec3 0.0 0.0 (-1.0)

samples :: Int
samples = 1



--
--  End of camera variables
--

-- A rudimentary cornell box.
facesCornell :: [Face]
facesCornell = loadObj "mtllib cornell_simple.mtl\no Cube\nv -4.000000 -4.000000 4.000000\nv -4.000000 4.000000 4.000000\nv -4.000000 -4.000000 -4.000000\nv -4.000000 4.000000 -4.000000\nv 4.000000 -4.000000 4.000000\nv 4.000000 4.000000 4.000000\nv 4.000000 -4.000000 -4.000000\nv 4.000000 4.000000 -4.000000\nvn -1.0000 0.0000 0.0000\nvn 1.0000 0.0000 0.0000\nvn 0.0000 0.0000 1.0000\nvn 0.0000 -1.0000 0.0000\nvn 0.0000 1.0000 0.0000\nusemtl Green\ns off\nf 2//1 3//1 1//1\nf 2//1 4//1 3//1\nusemtl Red\nf 8//2 5//2 7//2\nf 8//2 6//2 5//2\nusemtl White\nf 6//3 1//3 5//3\nf 7//4 1//4 3//4\nf 4//5 6//5 8//5\nf 6//3 2//3 1//3\nf 7//4 5//4 1//4\nf 4//5 2//5 6//5\no Cube.001\nv 1.032842 -4.123214 2.313145\nv 1.032842 -2.123214 2.313145\nv -0.381372 -4.123214 0.898931\nv -0.381372 -2.123214 0.898931\nv 2.447055 -4.123214 0.898931\nv 2.447055 -2.123214 0.898931\nv 1.032842 -4.123214 -0.515282\nv 1.032842 -2.123210 -0.515282\nvn -0.7071 0.0000 0.7071\nvn -0.7071 0.0000 -0.7071\nvn 0.7071 0.0000 -0.7071\nvn 0.7071 0.0000 0.7071\nvn 0.0000 -1.0000 0.0000\nvn 0.0000 1.0000 0.0000\nusemtl Blue\ns off\nf 10//6 11//6 9//6\nf 12//7 15//7 11//7\nf 15//8 14//8 13//8\nf 14//9 9//9 13//9\nf 15//10 9//10 11//10\nf 12//11 14//11 16//11\nf 10//6 12//6 11//6\nf 12//7 16//7 15//7\nf 15//8 16//8 14//8\nf 14//9 10//9 9//9\nf 15//10 13//10 9//10\nf 12//11 10//11 14//11\no Cube.002\nv -3.520742 -4.092613 1.154484\nv -3.520742 0.000255 1.154484\nv -2.625176 -4.092613 -0.633800\nv -2.625176 0.000255 -0.633800\nv -1.732458 -4.092613 2.050050\nv -1.732458 0.000255 2.050050\nv -0.836891 -4.092613 0.261766\nv -0.836891 0.000255 0.261766\nvn -0.8941 0.0000 -0.4478\nvn 0.4478 0.0000 -0.8941\nvn 0.8941 0.0000 0.4478\nvn -0.4478 0.0000 0.8941\nvn 0.0000 -1.0000 0.0000\nvn 0.0000 1.0000 0.0000\nusemtl White\ns off\nf 18//12 19//12 17//12\nf 20//13 23//13 19//13\nf 24//14 21//14 23//14\nf 22//15 17//15 21//15\nf 23//16 17//16 19//16\nf 20//17 22//17 24//17\nf 18//12 20//12 19//12\nf 20//13 24//13 23//13\nf 24//14 22//14 21//14\nf 22//15 18//15 17//15\nf 23//16 21//16 17//16\nf 20//17 18//17 22//17\no Plane\nv -1.000000 3.900000 1.000000\nv 1.000000 3.900000 1.000000\nv -1.000000 3.900000 -1.000000\nv 1.000000 3.900000 -1.000000\nvn 0.0000 1.0000 0.0000\nusemtl EWhite\ns off\nf 26//18 27//18 25//18\nf 26//18 28//18 27//18"
-- A cornell box with a mirror in the corner.
facesCornellMirror :: [Face]
facesCornellMirror = loadObj "# Blender v2.82 (sub 5) OBJ File: 'cornell.blend'\n# www.blender.org\nmtllib cornell_mirror.mtl\no Cube\nv -4.000000 -4.000000 4.000000\nv -4.000000 4.000000 4.000000\nv -4.000000 -4.000000 -4.000000\nv -4.000000 4.000000 -4.000000\nv 4.000000 -4.000000 4.000000\nv 4.000000 4.000000 4.000000\nv 4.000000 -4.000000 -4.000000\nv 4.000000 4.000000 -4.000000\nvn -1.0000 0.0000 0.0000\nvn 1.0000 0.0000 0.0000\nvn 0.0000 0.0000 1.0000\nvn 0.0000 -1.0000 0.0000\nvn 0.0000 1.0000 0.0000\nusemtl Green\ns off\nf 2//1 3//1 1//1\nf 2//1 4//1 3//1\nusemtl Red\nf 8//2 5//2 7//2\nf 8//2 6//2 5//2\nusemtl White\nf 6//3 1//3 5//3\nf 7//4 1//4 3//4\nf 4//5 6//5 8//5\nf 6//3 2//3 1//3\nf 7//4 5//4 1//4\nf 4//5 2//5 6//5\no Cube.001\nv 1.032842 -4.123214 2.313145\nv 1.032842 -2.123214 2.313145\nv -0.381372 -4.123214 0.898931\nv -0.381372 -2.123214 0.898931\nv 2.447055 -4.123214 0.898931\nv 2.447055 -2.123214 0.898931\nv 1.032842 -4.123214 -0.515282\nv 1.032842 -2.123210 -0.515282\nvn -0.7071 0.0000 0.7071\nvn -0.7071 0.0000 -0.7071\nvn 0.7071 0.0000 -0.7071\nvn 0.7071 0.0000 0.7071\nvn 0.0000 -1.0000 0.0000\nvn 0.0000 1.0000 0.0000\nusemtl Blue\ns off\nf 10//6 11//6 9//6\nf 12//7 15//7 11//7\nf 15//8 14//8 13//8\nf 14//9 9//9 13//9\nf 15//10 9//10 11//10\nf 12//11 14//11 16//11\nf 10//6 12//6 11//6\nf 12//7 16//7 15//7\nf 15//8 16//8 14//8\nf 14//9 10//9 9//9\nf 15//10 13//10 9//10\nf 12//11 10//11 14//11\no Cube.002\nv -3.520742 -4.092613 1.154484\nv -3.520742 0.000255 1.154484\nv -2.625176 -4.092613 -0.633800\nv -2.625176 0.000255 -0.633800\nv -1.732458 -4.092613 2.050050\nv -1.732458 0.000255 2.050050\nv -0.836891 -4.092613 0.261766\nv -0.836891 0.000255 0.261766\nvn -0.8941 0.0000 -0.4478\nvn 0.4478 0.0000 -0.8941\nvn 0.8941 0.0000 0.4478\nvn -0.4478 0.0000 0.8941\nvn 0.0000 -1.0000 0.0000\nvn 0.0000 1.0000 0.0000\nusemtl White\ns off\nf 18//12 19//12 17//12\nf 20//13 23//13 19//13\nf 24//14 21//14 23//14\nf 22//15 17//15 21//15\nf 23//16 17//16 19//16\nf 20//17 22//17 24//17\nf 18//12 20//12 19//12\nf 20//13 24//13 23//13\nf 24//14 22//14 21//14\nf 22//15 18//15 17//15\nf 23//16 21//16 17//16\nf 20//17 18//17 22//17\no Plane\nv -1.000000 3.900000 1.000000\nv 1.000000 3.900000 1.000000\nv -1.000000 3.900000 -1.000000\nv 1.000000 3.900000 -1.000000\nvn 0.0000 1.0000 0.0000\nusemtl EWhite\ns off\nf 26//18 27//18 25//18\nf 26//18 28//18 27//18\no Plane.002\nv 4.227553 0.141747 2.050061\nv 1.022640 0.700466 3.918530\nv 3.204612 3.633348 -0.748627\nv -0.000301 4.192067 1.119842\nvn -0.4696 -0.6318 -0.6166\nusemtl Mirror\ns off\nf 30//19 31//19 29//19\nf 30//19 32//19 31//19\n"


main :: IO ()
main = do 
    -- writeFile "output" $ show $ ...
    gen <- newStdGen
    BL.writeFile "render.bmp" 
        (seq facesCornellMirror (BL.pack (makeBMP (floor widthRender, floor heightRender) 
            (traceManySeededSPPThreaded 10 facesCornellMirror (widthRender, heightRender, degToRad (fovRender / 2.0)) (Ray campos (normalize camdir)) samples gen) 
            )))
        
        
-- Path: "D:\\Users\\vikto_000\\Documents\\gh-repos\\fp-pract-tasks-1920-Viktorsmg\\RT\\hask_rt.hs"


-- General documentation:
-- All functions have what they do written out above them.
-- To get started, somehow plug an obj file* (which is really just a text file) into loadObj, which will give you a [Face]

-- then plug into traceSeededSPPThreaded: 
-- <Size of ray chunk per thread> 
-- <Those [Face]> 
-- (<camera width>, <camera height>, <camera fov>) 
-- (Ray <position vector> <direction vector>)
-- <sample count>
-- <random number generator>

-- and use makeBMP on the resulting color array ([Vec3]), and export it into a bmp file using the Data.Bytestring.Laze functions.

-- *As OBJ files lack proper material descriptions, use the special material names for the obj files to define materials with the specific properties.
-- (Line 441)
-- Or, add your own!

-- Immediately above this are some camera variables you can play around with 
-- (Line 606)

-- To change the background color, go to 
-- (Line 136)
-- Note that the background also emits light.

-- Two OBJ files are included:
-- a basic cornell box
-- a cornell box with a mirror placed in a corner such that it gives a somewhat interesting reflection.