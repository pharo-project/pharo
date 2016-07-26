accessing
subPixelAntiAliasing
	^ self useSubPixelAntiAliasing and: [ subPixelAntiAliasing ifNil: [ false ] ]