Extension { #name : #ClyClassDefinitionEditorToolMorph }

{ #category : #'*FluidClassDefinition' }
ClyClassDefinitionEditorToolMorph >> parseClassDefinition [
	^ CDFluidClassDefinitionParser isOn 
		ifTrue: [ CDFluidClassDefinitionParser parse: self editingText ]
		ifFalse: [ CDClassDefinitionParser parse: self editingText ]
]
