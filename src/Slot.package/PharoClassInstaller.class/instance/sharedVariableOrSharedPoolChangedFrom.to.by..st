notifications
sharedVariableOrSharedPoolChangedFrom: oldClass to: newClass by: classModification 

	self 
		classDefinitionChangedFrom: oldClass 
		to: newClass 
		by: classModification.
	
	"for every class var in the classModification: if this exists in oldClass, copy value over"
	classModification classVariables do: [ :each | 
		(oldClass hasClassVarNamed: each name) ifTrue: [
			each write: (oldClass classVariableNamed: each name) value  ]].
	
	"Update subclasses. No need to propagate the modification to them."
	oldClass subclasses do: [ :each |
		each superclass: newClass.
		newClass addSubclass: each.
		each classLayout slotScope ifNotEmpty: [ :scope | scope parentScope: newClass classLayout slotScope ] ].