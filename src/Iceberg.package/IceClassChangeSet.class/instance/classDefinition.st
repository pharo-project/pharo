accessing
classDefinition
	^ classDefinition ifNil: [ self analyseChanges. classDefinition ]
