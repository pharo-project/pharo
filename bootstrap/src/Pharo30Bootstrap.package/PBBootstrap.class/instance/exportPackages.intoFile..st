preparation
exportPackages: packageList intoFile: aFileName

	self
		exportPackages: packageList
		usingInitializeScript: ''
		intoFile: aFileName