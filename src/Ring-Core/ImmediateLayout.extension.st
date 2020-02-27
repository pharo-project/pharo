Extension { #name : #ImmediateLayout }

{ #category : #'*Ring-Core' }
ImmediateLayout >> asRingMinimalDefinitionIn: anRGEnvironment [

	^ anRGEnvironment backend definitionFor: self ifAbsentRegister: [
		RGImmediateLayout parent: (self host asRingMinimalDefinitionIn: anRGEnvironment)]

]
