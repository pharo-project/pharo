initialization
initializePresenter
	super initializePresenter.
	self location: self repository location.
	self codeDirectory: self repository subdirectory.