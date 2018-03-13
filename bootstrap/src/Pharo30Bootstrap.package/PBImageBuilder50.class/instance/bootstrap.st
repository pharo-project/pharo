running
bootstrap

	self
		initializeBootstrapEnvironment;
		createVMStubs;
		flushNewSpace;
		createInitialObjects;
		createClasses;
		installMethods;
		installExtensionMethods;
		initializeImage