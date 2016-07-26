private-primitives
primSetTransform: matrixWordArray delta: deltaWordArray
	"matrix is 16.16 fixed point
		x' = x*m[0] + y*m[1]                                 
		y' = x*m[2] + y*yy[3]
	delta is 26.6 fixed point
		x' = x + d[0]
		y' = y + d[1]
	"
	<primitive: 'primitiveSetTransform' module: 'FT2Plugin'>
	^self primitiveFailed.