refactory-changes
renameClassVariable: oldName to: newName in: aClass
	^ self addChange: (RBRefactoryChangeManager changeFactory renameClassVariable: oldName to: newName in: aClass)