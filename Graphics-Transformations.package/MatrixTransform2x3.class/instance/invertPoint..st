transforming points
invertPoint: aPoint 
	"Transform aPoint from global coordinates into local coordinates"
	| x y det a11 a12 a21 a22 detX detY |
	x := aPoint x asFloat - self a13.
	y := aPoint y asFloat - self a23.
	a11 := self a11.
	a12 := self a12.
	a21 := self a21.
	a22 := self a22.
	det := a11 * a22 - (a12 * a21).
	det = 0.0 ifTrue: [ ^ 0 @ 0 ].	"So we have at least a valid result"
	det := 1.0 / det.
	detX := x * a22 - (a12 * y).
	detY := a11 * y - (x * a21).
	^ (detX * det) @ (detY * det)