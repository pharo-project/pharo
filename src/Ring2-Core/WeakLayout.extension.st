Extension { #name : #WeakLayout }

{ #category : #'*Ring2-Core' }
WeakLayout >> asRing2MinimalDefinitionIn: aRG2Environment [

	^ aRG2Environment backend definitionFor: self ifAbsentRegister: [
		RG2WeakLayoutDefinition parent: (self host asRing2MinimalDefinitionIn: aRG2Environment)]

]
