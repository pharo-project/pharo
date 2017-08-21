refactory-changes
renameClass: class to: newName 
	^ self addChange: (RBRefactoryChangeManager changeFactory renameClass: class to: newName)