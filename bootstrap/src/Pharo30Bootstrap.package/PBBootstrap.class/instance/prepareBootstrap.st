preparation
prepareBootstrap
	
	self
		exportKernelProtocols;
		exportAllPackagesIntoMcz;
		exportMonticelloInStFile;
		exportSUnitInStFile;
		exportCoreTestsInStFile;
		exportFileSystemInStFile;
		exportDeprecatedInStFile;
		exportJenkinsToolsInStFile.