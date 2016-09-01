initialization
initializeActionToPerformOnTextModel
	
	| selectedItem |
	sourceCodeModel acceptBlock:  [ :text | 	
		selectedItem := criticsModel selectedItem sourceAnchor entity.
		(selectedItem isCompiledMethod and: [ rule isTransformationRule not ])
			ifTrue: [ selectedItem methodClass compile: text ] 
			ifFalse: [ Smalltalk compiler evaluate:  text ]]
	
