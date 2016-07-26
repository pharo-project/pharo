private
linearLoadPackageSpecs: packageSpecs repositories: repositories
	
	self loaderPolicy 
		pushLinearLoadDirectivesDuring: [ super linearLoadPackageSpecs: packageSpecs repositories: repositories ]
		 for: self