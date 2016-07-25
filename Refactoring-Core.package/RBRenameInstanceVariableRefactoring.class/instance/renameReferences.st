transforming
renameReferences
	| replacer |
	replacer := RBParseTreeRewriter 
				rename: variableName
				to: newName
				handler: 
					[self 
						refactoringError: ('<1s> is already defined as a method or block temporary <n> variable in this class or one of its subclasses' 
								expandMacrosWith: newName)].
	self 
		convertClasses: class withAllSubclasses
		select: [:aClass | aClass whichSelectorsReferToInstanceVariable: variableName]
		using: replacer