notifications
shallowClassDefinitionChangedFrom: oldClass to: newClass by: classModification
	" Copy over the method organization "
	" Update the superclass links "
	self 
		shallowCopyMethodsFrom: oldClass to: newClass using: classModification;
		basicClassDefinitionChangedFrom: oldClass to: newClass using: classModification