private - layout
gradientRamp
	^ gradientRamp ifNil:[{0.0 -> originalColor muchLighter. 1.0 -> originalColor twiceDarker}]