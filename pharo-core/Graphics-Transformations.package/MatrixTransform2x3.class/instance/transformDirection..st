transforming points
transformDirection: aPoint 
	"Transform aPoint from local coordinates into global coordinates"
	| x y |
	x := aPoint x * self a11 + (aPoint y * self a12).
	y := aPoint x * self a21 + (aPoint y * self a22).
	^ x @ y