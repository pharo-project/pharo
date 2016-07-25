transforming
transform
	self model 
		renameClass: class
		to: newName
		around: [self renameReferences]