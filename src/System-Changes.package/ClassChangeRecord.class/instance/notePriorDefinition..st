definition
notePriorDefinition: oldClass

	oldClass ifNil: [^ self].
	priorDefinition ifNil: [priorDefinition := oldClass definition]