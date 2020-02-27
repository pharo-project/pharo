Extension { #name : #ByteLayout }

{ #category : #'*Ring-Core' }
ByteLayout >> asRingMinimalDefinitionIn: anRGEnvironment [

	^ anRGEnvironment backend definitionFor: self ifAbsentRegister: [
		RGByteLayout parent: (self host asRingMinimalDefinitionIn: anRGEnvironment)]

]
