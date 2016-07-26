composing
composedWithLocal: aTransformation into: result 
	"Return the composition of the receiver and the local transformation passed in.
	Store the composed matrix into result."
	| a11 a12 a13 a21 a22 a23 b11 b12 b13 b21 b22 b23 matrix |
	<primitive: 'primitiveComposeMatrix' module: 'Matrix2x3Plugin'>
	matrix := aTransformation asMatrixTransform2x3.
	a11 := self a11.
	b11 := matrix a11.
	a12 := self a12.
	b12 := matrix a12.
	a13 := self a13.
	b13 := matrix a13.
	a21 := self a21.
	b21 := matrix a21.
	a22 := self a22.
	b22 := matrix a22.
	a23 := self a23.
	b23 := matrix a23.
	result a11: a11 * b11 + (a12 * b21).
	result a12: a11 * b12 + (a12 * b22).
	result a13: a13 + (a11 * b13) + (a12 * b23).
	result a21: a21 * b11 + (a22 * b21).
	result a22: a21 * b12 + (a22 * b22).
	result a23: a23 + (a21 * b13) + (a22 * b23).
	^ result