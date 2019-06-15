module Test where
import Hylogen.WithHylide
import Hylogen.Types

data Ray = Ray Vec3 Vec3

output :: Program
output = toProgram color

color :: Vec4
color = vec4 (x_ col, y_ col, z_ col, 1)
  where
    col = rayColor $ Ray origin dir
    origin = vec3 (0, 0, 0)
    dir = vec3 (x_ uvN + x_ mouse, y_ uvN + y_ mouse, -1)

rayColor :: Ray -> Vec3
rayColor ray@(Ray origin dir) = col
	where
		col = sel (hitSphere originS radiusS ray) (vec3 (1, 0, 0)) sky
		originS = vec3 (0,0,5)
		radiusS = 1
		sky = (vec3 (1, 1, 1) ^* (1.0 - t)) + (vec3 (0.5, 0.7, 1.0) ^* t)
		t = 0.5*(y_ dir + 1.0)

hitSphere :: Vec3 -> Vec1 -> Ray -> Booly
hitSphere center radius (Ray origin dir) = gt discriminant 0
	where
		discriminant = b*b - 4*a*c
		c = (oc <.> oc) - radius * radius
		b = 2 * (oc <.> dir)
		a = (dir <.> dir)
		oc = origin - center

