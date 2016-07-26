transforming
transformBy: aTransform
	aTransform ifNil:[^self].
	transform 
		ifNil:[transform := aTransform]
		ifNotNil:[transform := transform composedWithLocal: aTransform]