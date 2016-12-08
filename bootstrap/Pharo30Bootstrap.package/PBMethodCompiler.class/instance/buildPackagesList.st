selection
buildPackagesList
	| classes |
	classes := builder behaviorDefinitions.
	^((Smalltalk at: #MCWorkingCopy ifAbsent: [ ^#() ] )
		allManagers
			select: [ :m | classes anySatisfy: [ :c | m packageInfo includesClassNamed: c name ] ]
			thenCollect: [ :m | m packageInfo packageName asUppercase ]) asSet asOrderedCollection.