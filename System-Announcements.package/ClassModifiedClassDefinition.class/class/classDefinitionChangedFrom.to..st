instance creation
classDefinitionChangedFrom: oldClass to: newClass
	^self new
			oldClassDefinition: oldClass;
			newClassDefinition: newClass;
			yourself