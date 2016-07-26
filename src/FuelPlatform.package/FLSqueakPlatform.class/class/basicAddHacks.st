private-hacks
basicAddHacks
	super basicAddHacks.
	
	self
		addTraits;
		addEnvironment;
		addNativeBoostComponents;
		addContext;
		addClusters;
		addPackageManifest;
		addAbstractBinaryFileStream