visitor
visitBehaviorNameChange: aClassRenamedChange

	^	(self displayOperation: 'Rename'), 
		(self displayBehavior: aClassRenamedChange oldName), 
		(self displayOperation: 'as'), 
		(self displayString: aClassRenamedChange newName)