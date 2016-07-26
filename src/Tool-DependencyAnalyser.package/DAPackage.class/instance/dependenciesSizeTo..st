dependencies
dependenciesSizeTo: aPackage
	| dependency |
	dependency := dependencies detect: [ :each | each target packageName = aPackage packageName ].
	^ dependency ifNotNil: [ dependency numberOfDependencies ] ifNil: [ 0 ]