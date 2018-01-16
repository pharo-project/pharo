Extension { #name : #Slot }

{ #category : #'*Ring2-Core' }
Slot >> asRing2MinimalDefinitionIn: aRG2Environment [

	^ self asRing2MinimalDefinitionIn: aRG2Environment parent: (self definingClass classLayout asRing2MinimalDefinitionIn: aRG2Environment)
]

{ #category : #'*Ring2-Core' }
Slot >> asRing2MinimalDefinitionIn: aRG2Environment parent: aRG2LayoutDefinition [

	^ aRG2Environment backend definitionFor: self ifAbsentRegister: [
		| def |
		def := RG2UnknownSlotDefinition named: self name asSymbol parent: aRG2LayoutDefinition.
		def pvtExpression: (self printString copyAfter: $>) trimmed.
		def ].
]
