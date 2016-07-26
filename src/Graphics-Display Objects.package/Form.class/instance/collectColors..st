converting
collectColors: aBlock
	"Create a new copy of the receiver with all the colors transformed by aBlock"
	^self collectPixels:[:pv|
		(aBlock value: (Color colorFromPixelValue: pv depth: self depth)) 
			pixelValueForDepth: self depth.
	].