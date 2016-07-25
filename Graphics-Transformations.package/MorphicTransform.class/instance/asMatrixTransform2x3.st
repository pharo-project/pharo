converting
asMatrixTransform2x3
	^((MatrixTransform2x3 withRotation: angle radiansToDegrees negated) composedWithLocal:
		(MatrixTransform2x3 withScale: scale))
			offset: offset negated