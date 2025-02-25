Extension { #name : 'Slot' }

{ #category : '*Ring-Core' }
Slot >> asRingMinimalDefinitionIn: anRGEnvironment [
	^ anRGEnvironment backend definitionFor: self ifAbsentRegister: [
		RGUnknownSlot named: self name asSymbol parent: (self definingClass classLayout asRingMinimalDefinitionIn: anRGEnvironment)]
]
