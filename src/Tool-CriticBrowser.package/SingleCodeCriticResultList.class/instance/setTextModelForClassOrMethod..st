initialization
setTextModelForClassOrMethod: aCritique

	| entity |
	entity := aCritique sourceAnchor entity.
	self flag: 'Another hack. We need a better way to display entities (because now we may get not only classes and methods. Maybe something like GTInspector presentations could work)'.
	sourceCodeModel text: (
		(entity respondsTo: #definition)
			ifTrue: [ entity definition ]
			ifFalse: [ entity asString ]).
	sourceCodeModel aboutToStyle: true.
	sourceCodeModel behavior: (
		entity isCompiledMethod
			ifTrue: [ entity methodClass ]
			ifFalse: [ nil ]).
	aCritique sourceAnchor providesInterval ifTrue: [ 
		sourceCodeModel setSelection: aCritique sourceAnchor interval ]
	
	
					