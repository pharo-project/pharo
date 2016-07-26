accessing
foundationBrick

	^ (self owner isKindOf: GLMFoundationBrick)
		ifTrue: [ self owner ]
		ifFalse: [ self ]