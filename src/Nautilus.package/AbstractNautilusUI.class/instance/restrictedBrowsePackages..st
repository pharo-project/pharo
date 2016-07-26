menus behavior
restrictedBrowsePackages: packages
	| newEnvironment |
	newEnvironment := self browsedEnvironment forPackages: packages.
	 self model class 
		openOnPackage: self selectedPackage inEnvironment: newEnvironment