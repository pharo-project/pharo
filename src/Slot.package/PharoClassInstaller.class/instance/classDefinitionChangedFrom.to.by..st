notifications
classDefinitionChangedFrom: oldClass to: newClass by: classModification
	
	self
		copyMethodsFrom: oldClass to: newClass using: classModification;
		basicClassDefinitionChangedFrom: oldClass to: newClass using: classModification;
		fixClassBindings: newClass
	