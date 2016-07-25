querying
hasPackageNamed: aName
	" self hasPackageNamed: 'ConfigurationOfFuel' "
	
	^ MCWorkingCopy allManagers
		detect: [ :each | each packageName = aName ]
		ifNone: [ false ].