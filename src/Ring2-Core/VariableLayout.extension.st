Extension { #name : #VariableLayout }

{ #category : #'*Ring2-Core' }
VariableLayout >> asRing2MinimalDefinitionIn: aRG2Environment [

	^ aRG2Environment backend definitionFor: self ifAbsentRegister: [
		RG2VariableLayoutDefinition parent: (self host asRing2MinimalDefinitionIn: aRG2Environment)]

]
