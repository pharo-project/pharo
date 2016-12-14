bootstrap
initializeImage
	
	self initializeClasses.
	self initializeSessionManager.
	self patchUnhandledErrorManagement.
	self initializeSpecialSelectors.
	self initializeDefaultCodeFormatter. 