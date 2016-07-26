transforming
renameReferences
	| replacer subclasses |
	replacer := RBParseTreeRewriter 
		rename: variableName
		to: newName
		handler: 
			[ self refactoringError: ('<1s> is already defined as a method or block temporary <n> variable in this class or one of its subclasses' expandMacrosWith: newName) ].
	subclasses := class withAllSubclasses asSet.
	subclasses addAll: class theMetaClass withAllSubclasses.
	self 
		convertClasses: subclasses
		select: [ :aClass | aClass whichSelectorsReferToClassVariable: variableName ]
		using: replacer