annotations
traitCompositionSource
	"Retrieves aString representing the used traits"

	^self annotationNamed: self class traitCompositionSourceKey
		ifAbsentPut:[  '{}'  ]