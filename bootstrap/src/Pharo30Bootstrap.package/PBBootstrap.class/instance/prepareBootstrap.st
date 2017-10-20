preparation
prepareBootstrap
	
	self
		exportKernelProtocols;
		exportKernelPackageNames;
		exportAllPackagesIntoMcz;
		exportMonticelloInStFile;
		exportMultilingualInStFile;
		exportSUnitInStFile;
		exportCoreTestsInStFile;
		exportFileSystemInStFile;
		exportDeprecatedInStFile;
		exportJenkinsToolsInStFile.