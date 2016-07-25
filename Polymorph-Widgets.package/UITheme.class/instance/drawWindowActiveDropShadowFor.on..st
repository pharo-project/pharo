border-styles
drawWindowActiveDropShadowFor: aSystemWindow on: aCanvas
	"Draw the active drop shadow for the given window."
	
	
	^self windowActiveDropShadowStyle == #diffuse
		ifTrue: [self drawWindowDiffuseDropShadowFor: aSystemWindow on: aCanvas]
		ifFalse: [self drawWindowPlainDropShadowFor: aSystemWindow on: aCanvas]