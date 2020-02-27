Extension { #name : #CompiledMethodLayout }

{ #category : #'*Ring-Core' }
CompiledMethodLayout >> asRingMinimalDefinitionIn: anRGEnvironment [

	^ anRGEnvironment backend definitionFor: self ifAbsentRegister: [
		RGCompiledMethodLayout parent: (self host asRingMinimalDefinitionIn: anRGEnvironment)]

]
