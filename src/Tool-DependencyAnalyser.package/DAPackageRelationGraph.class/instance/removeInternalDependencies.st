actions
removeInternalDependencies
	"Remove all dependencies from a package to itself"
	
	self includedPackages
		do: [ :each | each removeAllInternal ]
		displayingProgress: 'Removing internal Dependencies'