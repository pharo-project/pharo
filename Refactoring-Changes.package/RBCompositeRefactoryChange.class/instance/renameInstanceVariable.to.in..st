refactory-changes
renameInstanceVariable: oldName to: newName in: aClass
	^ self addChange: (RBRenameInstanceVariableChange rename: oldName to: newName in: aClass)