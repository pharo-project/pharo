transforming
colorTransformBy: aColorTransform
	aColorTransform ifNil:[^self].
	colorTransform 
		ifNil:[colorTransform := aColorTransform]
		ifNotNil:[colorTransform := colorTransform composedWithLocal: aColorTransform]