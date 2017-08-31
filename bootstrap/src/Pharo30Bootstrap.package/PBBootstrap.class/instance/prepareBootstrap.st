preparation
prepareBootstrap
	
	self
		exportKernelProtocols;
		exportSUnitProtocols;
		exportKernelPackageNames;
		exportSUnitPackageNames;
		exportAllPackagesIntoMcz;
		exportMonticelloInStFile;
		exportMultilingualInStFile;
		exportSUnitInStFile;
		exportCoreTestsInStFile;
		exportFileSystemInStFile;
		exportDeprecatedInStFile;
		exportJenkinsToolsInStFile.