Extension { #name : #WordLayout }

{ #category : #'*Ring-Core' }
WordLayout >> asRingMinimalDefinitionIn: anRGEnvironment [

	^ anRGEnvironment backend definitionFor: self ifAbsentRegister: [
		RGWordLayout parent: (self host asRingMinimalDefinitionIn: anRGEnvironment)]

]
