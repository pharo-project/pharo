Extension { #name : #CompiledMethod }

{ #category : #'*Ring2-Core' }
CompiledMethod >> asRing2MinimalDefinitionIn: aRG2Environment [

	^ aRG2Environment backend definitionFor: self ifAbsentRegister: [
		RG2MethodDefinition named: self selector asSymbol parent: (self methodClass asRing2MinimalDefinitionIn: aRG2Environment)]

]

{ #category : #'*Ring2-Core' }
CompiledMethod >> ensureRing2DefinitionIn: aRG2Environment [

	| parentModel |
	
	parentModel := (self methodClass ensureRing2DefinitionIn: aRG2Environment).
	
	^ parentModel methods 
		detect: [:each | each selector = self selector]
		ifNone: [ 
			| methodModel |
			methodModel := self asRing2MinimalDefinitionIn: aRG2Environment.
			parentModel addLocalMethod: methodModel.
			methodModel ]	
]
