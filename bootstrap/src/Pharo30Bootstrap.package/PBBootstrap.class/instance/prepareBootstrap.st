preparation
prepareBootstrap
	
	self
		exportKernelProtocols;
		exportAllPackagesIntoMcz;
		exportMonticelloInStFile;
		exportSUnitInStFile;
		exportCoreTestsInStFile;
		exportHudsonBuildToolsInStFile.