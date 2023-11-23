Extension { #name : 'DoubleWordLayout' }

{ #category : '*Ring-Core' }
DoubleWordLayout >> asRingMinimalDefinitionIn: anRGEnvironment [

	^ anRGEnvironment backend definitionFor: self ifAbsentRegister: [
		RGDoubleWordLayout parent: (self host asRingMinimalDefinitionIn: anRGEnvironment)]
]
