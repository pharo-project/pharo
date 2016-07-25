private
atomicLoadPackageSpecs: packageSpecs repositories: repositories

	self loaderPolicy 
		pushAtomicLoadDirectivesDuring: [ super linearLoadPackageSpecs: packageSpecs repositories: repositories ]
		for: self