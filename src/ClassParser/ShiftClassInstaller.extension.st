Extension { #name : #ShiftClassInstaller }

{ #category : #'*ClassParser' }
ShiftClassInstaller >> makeFromString: aStringClassDefinition [
	
	builder buildFromAST: (CDClassDefinitionParser parse: aStringClassDefinition).

	^self make
]
