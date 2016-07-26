geometry
computedHeightFrom: aRectangle
	^ row ifNil: [0] ifNotNil: [row height]