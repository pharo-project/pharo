actions
commitWithMessage: commitMessage
	self repository 
		savePackage: self package name
		withMessage: commitMessage