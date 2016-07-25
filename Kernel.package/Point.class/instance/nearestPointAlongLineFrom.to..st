point functions
nearestPointAlongLineFrom: p1 to: p2 
	"Note this will give points beyond the endpoints."
	
	| x21 y21 t x1 y1 |
	p1 x = p2 x ifTrue: [ ^ p1 x @ y ].
	p1 y = p2 y ifTrue: [ ^ x @ p1 y ].
	x1 := p1 x asFloat.
	y1 := p1 y asFloat.
	x21 := p2 x asFloat - x1.
	y21 := p2 y asFloat - y1.
	t := ((y asFloat - y1) / x21 + ((x asFloat - x1) / y21)) / (x21 / y21 + (y21 / x21)).
	^ (x1 + (t * x21)) @ (y1 + (t * y21))
	