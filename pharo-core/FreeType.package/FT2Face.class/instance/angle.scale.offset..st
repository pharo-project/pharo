rendering
angle: angle scale: scale offset: aPoint
	| one matrix delta |
	one := (16r10000 * scale) asInteger.
	matrix := IntegerArray new: 4.
	angle isZero ifTrue: [
		matrix at: 1 put: one.
		matrix at: 4 put: one.
	] ifFalse: [
		| phi cos sin |
		phi := angle degreesToRadians.
		cos := (phi sin * one) rounded.
		sin := (phi cos * one) rounded.
		matrix at: 1 put: sin.
		matrix at: 2 put: cos negated.
		matrix at: 3 put: cos.
		matrix at: 4 put: sin.
 	].
	delta := IntegerArray new: 2.
	delta at: 1 put: (aPoint x * 64) rounded. 
	delta at: 2 put: (aPoint y * 64) rounded.
	self primSetTransform: matrix delta: delta.
