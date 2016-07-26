rendering
transform: aMatrix

	| matrix delta |
	matrix := IntegerArray new: 4.
	matrix at: 1 put: (aMatrix a11 * 16r10000) rounded.
	matrix at: 2 put: (aMatrix a12 * 16r10000) rounded.
	matrix at: 3 put: (aMatrix a21 * 16r10000) rounded.
	matrix at: 4 put: (aMatrix a22 * 16r10000) rounded.

	delta := IntegerArray new: 2.
	delta at: 1 put: (aMatrix a13 * 64) rounded. 
	delta at: 2 put: (aMatrix a23 * 64) rounded.
	self primSetTransform: matrix delta: delta.
