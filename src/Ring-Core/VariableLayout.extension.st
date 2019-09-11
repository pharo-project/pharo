Extension { #name : #VariableLayout }

{ #category : #'*Ring-Core' }
VariableLayout >> asRingMinimalDefinitionIn: anRGEnvironment [

	^ anRGEnvironment backend definitionFor: self ifAbsentRegister: [
		RGVariableLayout parent: (self host asRingMinimalDefinitionIn: anRGEnvironment)]

]
