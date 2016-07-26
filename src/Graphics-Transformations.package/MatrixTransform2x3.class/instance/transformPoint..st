transforming points
transformPoint: aPoint 
	"Transform aPoint from local coordinates into global coordinates"
	| x y |
	x := aPoint x * self a11 + (aPoint y * self a12) + self a13.
	y := aPoint x * self a21 + (aPoint y * self a22) + self a23.
	^ x @ y