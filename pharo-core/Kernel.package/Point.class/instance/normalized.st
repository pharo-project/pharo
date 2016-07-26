point functions
normalized
	"Optimized for speed"
	| r |
	r := (x * x + (y * y)) sqrt.
	^ (x / r) @ (y / r)