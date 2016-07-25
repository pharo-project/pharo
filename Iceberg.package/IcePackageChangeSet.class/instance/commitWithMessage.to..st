actions
commitWithMessage: commitMessage to: repository
	"It should not be necessary to receive the repository as an argument, I should get it
	from the package (which should be smarter)"
	repository 
		savePackage: self package packageName
		withMessage: commitMessage.
