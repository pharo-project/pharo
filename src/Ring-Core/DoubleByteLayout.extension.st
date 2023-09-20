Extension { #name : 'DoubleByteLayout' }

{ #category : '*Ring-Core' }
DoubleByteLayout >> asRingMinimalDefinitionIn: anRGEnvironment [

	^ anRGEnvironment backend definitionFor: self ifAbsentRegister: [
		RGDoubleByteLayout parent: (self host asRingMinimalDefinitionIn: anRGEnvironment)]
]
