as yet unclassified
defaultImageFormOfSize: aPoint
	"Answer a default preview image form."

	^(defaultImageForm isNil or: [defaultImageForm extent ~= aPoint])
		ifTrue: [defaultImageForm := Form extent: aPoint]
		ifFalse: [defaultImageForm]