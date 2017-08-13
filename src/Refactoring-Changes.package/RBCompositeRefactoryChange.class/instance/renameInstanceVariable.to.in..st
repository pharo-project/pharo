refactory-changes
renameInstanceVariable: oldName to: newName in: aClass
	^ self addChange: (RBRefactoryChangeManager changeFactory renameInstanceVariable: oldName to: newName in: aClass)