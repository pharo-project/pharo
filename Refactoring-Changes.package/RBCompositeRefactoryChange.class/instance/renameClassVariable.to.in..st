refactory-changes
renameClassVariable: oldName to: newName in: aClass
	^ self addChange: (RBRenameClassVariableChange rename: oldName to: newName in: aClass)