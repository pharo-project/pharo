testing
hasOutgoingDependencies: aPackage
	^ aPackage dependencies anySatisfy: [ :dependency | (self packages includes: (dependency target)) not ].