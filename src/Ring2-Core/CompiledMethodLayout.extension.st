Extension { #name : #CompiledMethodLayout }

{ #category : #'*Ring2-Core' }
CompiledMethodLayout >> asRing2MinimalDefinitionIn: aRG2Environment [

	^ aRG2Environment backend definitionFor: self ifAbsentRegister: [
		RG2CompiledMethodLayoutDefinition parent: (self host asRing2MinimalDefinitionIn: aRG2Environment)]

]
