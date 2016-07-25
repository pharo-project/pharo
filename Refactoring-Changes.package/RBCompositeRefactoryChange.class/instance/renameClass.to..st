refactory-changes
renameClass: class to: newName 
	^ self addChange: (RBRenameClassChange rename: class name to: newName)