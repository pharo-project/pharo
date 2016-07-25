transforming
rename: aClass name: aSymbol
	^ RBRenameClassRefactoring 
		model: self model
		rename: aClass
		to: aSymbol