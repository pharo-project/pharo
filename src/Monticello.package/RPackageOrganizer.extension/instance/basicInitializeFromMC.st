*Monticello-RPackage
basicInitializeFromMC

	self basicInitializeFromPackagesList: (self allManagers collect: [ :workingCopy | workingCopy packageName]).