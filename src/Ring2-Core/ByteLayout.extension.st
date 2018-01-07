Extension { #name : #ByteLayout }

{ #category : #'*Ring2-Core' }
ByteLayout >> asRing2MinimalDefinitionIn: aRG2Environment [

	^ aRG2Environment backend definitionFor: self ifAbsentRegister: [
		RG2ByteLayoutDefinition parent: (self host asRing2MinimalDefinitionIn: aRG2Environment)]

]
