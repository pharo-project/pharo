dependencies
packageDependencies
	^ self rootNode isCycle
		ifTrue: [ self rootNode content dependentPackagesFrom: self content ]
		ifFalse: [ self content dependencies ]