creating
duplicateClass: aClass withNewName: name
	| newClass |
	
	newClass := aClass duplicateClassWithNewName: name.
	self createdClasses add: newClass.
	^newClass
