Extension { #name : #Slot }

{ #category : #'*Ring-Core' }
Slot >> asRingMinimalDefinitionIn: anRGEnvironment [

	^ self asRingMinimalDefinitionIn: anRGEnvironment parent: (self definingClass classLayout asRingMinimalDefinitionIn: anRGEnvironment)
]

{ #category : #'*Ring-Core' }
Slot >> asRingMinimalDefinitionIn: anRGEnvironment parent: anRGLayout [

	^ anRGEnvironment backend definitionFor: self ifAbsentRegister: [
		| def |
		def := RGUnknownSlot named: self name asSymbol parent: anRGLayout.
		def ].
]
