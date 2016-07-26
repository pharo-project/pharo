initialization
setTextModelForClassOrMethod: aCritique

	| interval entity |
	entity := aCritique sourceAnchor entity.
	sourceCodeModel text: entity definition.
	sourceCodeModel aboutToStyle: true.
	sourceCodeModel behavior: (entity isClassOrTrait ifTrue: [ nil ] ifFalse: [entity methodClass]).
	aCritique sourceAnchor providesInterval ifTrue: [ 
		sourceCodeModel setSelection: aCritique sourceAnchor interval ]
	
	
					