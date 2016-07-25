protocol
removeDependencyNamed: aDependencyName
	"Remove a depedency from the configuration"
	
	(self dependencyNamed: aDependencyName) remove