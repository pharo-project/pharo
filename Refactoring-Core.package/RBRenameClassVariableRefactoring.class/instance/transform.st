transforming
transform
	class 
		renameClassVariable: variableName
		to: newName
		around: [self renameReferences]