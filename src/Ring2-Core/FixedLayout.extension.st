Extension { #name : #FixedLayout }

{ #category : #'*Ring2-Core' }
FixedLayout >> asRing2MinimalDefinitionIn: aRG2Environment [

	^ aRG2Environment backend definitionFor: self ifAbsentRegister: [
		RG2FixedLayoutDefinition parent: (self host asRing2MinimalDefinitionIn: aRG2Environment)]

]
