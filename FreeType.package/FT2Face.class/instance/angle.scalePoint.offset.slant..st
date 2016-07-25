rendering
angle: angle scalePoint: scalePoint offset: aPoint slant: slant
	| oneX oneY matrix delta slantOne|
	oneX := (16r10000 * scalePoint x) asInteger.
	oneY :=  (16r10000 * scalePoint y) asInteger.
	slantOne := (16r10000 * scalePoint x * slant) asInteger.
	matrix := IntegerArray new: 4.
	angle isZero ifTrue: [
		matrix at: 1 put: oneX.
		matrix at: 2 put: slantOne.
		matrix at: 4 put: oneY.
	] ifFalse: [
		| phi cos sin |
		phi := angle degreesToRadians.
		cos := (phi sin * oneX) rounded.
		sin := (phi cos * oneY) rounded.
		matrix at: 1 put: sin.
		matrix at: 2 put: cos negated.
		matrix at: 3 put: cos.
		matrix at: 4 put: sin.
 	].
	delta := IntegerArray new: 2.
	delta at: 1 put: (aPoint x * 64) rounded. 
	delta at: 2 put: (aPoint y * 64) rounded.
	self primSetTransform: matrix delta: delta.
