preparation
prepareBootstrap
	
	self
		exportKernelProtocols;
		exportAllPackagesIntoMcz;
		exportMonticelloInStFile;
		exportMultilingualInStFile;
		exportSUnitInStFile;
		exportCoreTestsInStFile;
		exportFileSystemInStFile;
		exportDeprecatedInStFile;
		exportJenkinsToolsInStFile.