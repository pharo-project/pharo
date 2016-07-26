repositories
configurationReferencesFrom: aRepository

	| map |
	map := Dictionary new.
	aRepository goferReferences do: [ :ref | 
		(ref packageName beginsWith: 'ConfigurationOf')
			ifTrue: [ | nameWithBranch |
				nameWithBranch := ref metacelloPackageNameWithBranch at: 2.
				(map at: nameWithBranch ifAbsent: [map at: nameWithBranch put: OrderedCollection new]) add: ref ]].
	^map