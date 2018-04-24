preparation
prepareBootstrap
	
	self
		exportKernelProtocols;
		exportKernelPackageNames;
		exportAllPackagesIntoMcz;
		exportMonticelloInStFile;
		
		prepareEnvironmentForHermes;
		generateHermesFiles;
		prepareEnvironmentForExport.