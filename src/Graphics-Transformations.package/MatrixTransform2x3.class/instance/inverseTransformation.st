accessing
inverseTransformation
	"Return the inverse transformation of the receiver.
	The inverse transformation is computed by first calculating
	the inverse offset and then computing transformations
	for the two identity vectors (1@0) and (0@1)"
	| r1 r2 r3 m |
	r3 := self invertPoint: 0 @ 0.
	r1 := (self invertPoint: 1 @ 0) - r3.
	r2 := (self invertPoint: 0 @ 1) - r3.
	m := self species new.
	m
		a11: r1 x;
		a12: r2 x;
		a13: r3 x;
		a21: r1 y;
		a22: r2 y;
		a23: r3 y.
	^ m