Extension { #name : #IndexedSlot }

{ #category : #'*Ring2-Core' }
IndexedSlot >> asRing2MinimalDefinitionIn: aRG2Environment [

	^ self class == IndexedSlot 
		ifTrue: [ aRG2Environment backend definitionFor: self ifAbsentRegister: [
			RG2IndexedSlotDefinition named: self name asSymbol parent: (self definingClass classLayout asRing2MinimalDefinitionIn: aRG2Environment)]]
		ifFalse: [ super asRing2MinimalDefinitionIn: aRG2Environment ]
]
