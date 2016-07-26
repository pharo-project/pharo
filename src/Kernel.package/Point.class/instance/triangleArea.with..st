geometry
triangleArea: b with: c
	"Returns twice the area of the oriented triangle (a, b, c), i.e., the area is positive if the triangle is oriented counterclockwise"
	^ b x - self x * (c y - self y) - (b y - self y * (c x - self x))