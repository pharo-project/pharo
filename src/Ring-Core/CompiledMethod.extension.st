Extension { #name : #CompiledMethod }

{ #category : #'*Ring-Core' }
CompiledMethod >> asRingMinimalDefinitionIn: anRGEnvironment [

	^ anRGEnvironment backend definitionFor: self ifAbsentRegister: [
		RGMethod named: self selector asSymbol parent: (self methodClass asRingMinimalDefinitionIn: anRGEnvironment)]

]

{ #category : #'*Ring-Core' }
CompiledMethod >> ensureRingDefinitionIn: anRGEnvironment [

	| parentModel |
	
	parentModel := (self methodClass ensureRingDefinitionIn: anRGEnvironment).
	
	^ parentModel methods 
		detect: [:each | each selector = self selector]
		ifNone: [ 
			| methodModel |
			methodModel := self asRingMinimalDefinitionIn: anRGEnvironment.
			parentModel addLocalMethod: methodModel.
			methodModel ]	
]
