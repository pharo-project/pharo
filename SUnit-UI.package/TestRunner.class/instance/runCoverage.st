actions
runCoverage
	| myPackages methods |
	myPackages := Set new.
	self addDeclaredPackagesUnderTestTo: myPackages.
	myPackages isEmpty ifTrue: 
		[ myPackages := self promptForPackages ].
	methods := OrderedCollection new.
	self 
		addMethodsUnderTestIn: myPackages
		to: methods.
	self excludeClassesNotUnderTestFrom: methods.
	methods isEmpty ifTrue: 
		[ ^ UIManager default inform: 'No methods found for coverage analysis.' ].
	self collectCoverageFor: methods
