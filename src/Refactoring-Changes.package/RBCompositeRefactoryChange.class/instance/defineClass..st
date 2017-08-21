refactory-changes
defineClass: aString 
	^ self addChange: (RBRefactoryChangeManager changeFactory addClassDefinition: aString)