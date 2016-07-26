manifest
toDoOf: aRule
	
	|critics rId rV mb |
	critics := self criticsOf: aRule. 
	rId := aRule class uniqueIdentifierName.
	rV := aRule class identifierMinorVersionNumber.
	^ critics select: [:critic |
		mb := (self manifestBuilderOf: critic).
		mb isNil 
			ifTrue: [ false ] 
			ifFalse: [ mb containsToDo: critic onRule:  rId version: rV ]]
	
	