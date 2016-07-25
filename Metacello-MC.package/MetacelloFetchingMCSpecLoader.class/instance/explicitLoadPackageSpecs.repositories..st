private
explicitLoadPackageSpecs: packageSpecs repositories: repositories
	
	| directive |
	directive := self loaderPolicy 
		pushExplicitLoadDirectivesDuring: [ super linearLoadPackageSpecs: packageSpecs repositories: repositories ]
		for: self.
	directive explicitLoadWithPolicy: self loaderPolicy.