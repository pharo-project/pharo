visitor
visitBehaviorNameChange: aClassRenamedChange
	^ (self displayBehavior: aClassRenamedChange oldName), 
		(self displayOperation: 'rename:'), 
		(self displayString: '#', aClassRenamedChange newName)