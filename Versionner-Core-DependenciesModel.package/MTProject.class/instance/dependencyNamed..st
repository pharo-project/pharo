accessing
dependencyNamed: aDependencyName
	"Get a depedency from its name"
	
	^ dependencies
		detect: [  :aDependency | (aDependency name =  aDependencyName) or: [ aDependency displayName =  aDependencyName ] ]
		ifNone: [ nil ]
