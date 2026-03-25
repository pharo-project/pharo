Extension { #name : 'Trait' }

{ #category : '*Ring-TraitsSupport' }
Trait >> asRingMinimalDefinitionIn: anRGEnvironment [

	^ anRGEnvironment backend definitionFor: self ifAbsentRegister: [
		RGTrait named: self name asSymbol parent: anRGEnvironment]
]
