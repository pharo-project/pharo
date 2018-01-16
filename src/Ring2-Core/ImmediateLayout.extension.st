Extension { #name : #ImmediateLayout }

{ #category : #'*Ring2-Core' }
ImmediateLayout >> asRing2MinimalDefinitionIn: aRG2Environment [

	^ aRG2Environment backend definitionFor: self ifAbsentRegister: [
		RG2ImmediateLayoutDefinition parent: (self host asRing2MinimalDefinitionIn: aRG2Environment)]

]
