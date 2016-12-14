running
bootstrap

	self initializeBootstrapEnvironment.
	self createVMStubs.
	self createInitialObjects.
	self createClasses.
	self installMethods.
	self initializeImage.