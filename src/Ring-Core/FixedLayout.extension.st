Extension { #name : #FixedLayout }

{ #category : #'*Ring-Core' }
FixedLayout >> asRingMinimalDefinitionIn: anRGEnvironment [

	^ anRGEnvironment backend definitionFor: self ifAbsentRegister: [
		RGFixedLayout parent: (self host asRingMinimalDefinitionIn: anRGEnvironment)]

]
