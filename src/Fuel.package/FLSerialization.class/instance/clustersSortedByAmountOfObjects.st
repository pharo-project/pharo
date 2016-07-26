debugging
clustersSortedByAmountOfObjects
	^ self clusters sorted: [ :a :b | a objects size > b objects size ]