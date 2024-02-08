Extension { #name : 'Package' }

{ #category : '*Ring-Core' }
Package >> asRingMinimalDefinitionIn: anRGEnvironment [

	^ anRGEnvironment backend definitionFor: self ifAbsentRegister: [
		RGPackage named: self name asSymbol parent: anRGEnvironment ]
]
