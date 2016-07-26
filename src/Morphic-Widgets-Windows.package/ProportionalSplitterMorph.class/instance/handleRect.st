accessing
handleRect

	^ Rectangle
		center: self bounds center 
		extent: (self splitsTopAndBottom
			ifTrue: [self handleSize transposed] 
			ifFalse: [self handleSize])