grouping
ensureClassChangeWrapperNamed: aClassName
	
	(classes includes: aClassName)
		ifFalse: [ classes add: aClassName.
				classChanges add: (PSMCClassChangeWrapper with: aClassName model: model ) ]