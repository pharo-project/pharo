testing
hasChanged
	^ diff dependentPackagesDiff
		detect: [ :each | each parent = self content and: [ each hasChanged ] ]
		ifFound: [ true ]
		ifNone: [ false ]