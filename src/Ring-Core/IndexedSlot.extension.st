Extension { #name : #IndexedSlot }

{ #category : #'*Ring-Core' }
IndexedSlot >> asRingMinimalDefinitionIn: anRGEnvironment [

	^ self class == IndexedSlot 
		ifTrue: [ anRGEnvironment backend definitionFor: self ifAbsentRegister: [
			RGIndexedSlot named: self name asSymbol parent: (self definingClass classLayout asRingMinimalDefinitionIn: anRGEnvironment)]]
		ifFalse: [ super asRingMinimalDefinitionIn: anRGEnvironment ]
]
