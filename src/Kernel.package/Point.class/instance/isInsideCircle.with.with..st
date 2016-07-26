geometry
isInsideCircle: a with: b with: c 
	"Returns true if self is inside the circle defined by the points a, b, c. See Guibas and Stolfi (1985) p.107"
	^ (a dotProduct: a)
		* (b triangleArea: c with: self) - ((b dotProduct: b)
			* (a triangleArea: c with: self)) + ((c dotProduct: c)
			* (a triangleArea: b with: self)) - ((self dotProduct: self)
			* (a triangleArea: b with: c)) > 0.0