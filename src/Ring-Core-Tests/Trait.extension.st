Extension { #name : 'Trait' }

{ #category : '*Ring-Core-Tests' }
Trait >> asRingMinimalDefinitionIn: anRGEnvironment [

	^ anRGEnvironment backend definitionFor: self ifAbsentRegister: [
		RGTrait named: self name asSymbol parent: anRGEnvironment]
]
