accessing - tree
roots
	| rootNames organizer|
	
	self hasSearch 
		ifTrue: [ ^ self filteredRoots ].
		
	organizer := RPackageOrganizer default.
	rootNames := (organizer packageNames collect: [ :name |
		name copyUpTo: $- ]) asSet asSortedCollection.
	
	^ rootNames collect: [  :name|
		organizer 
			packageNamed: name
			ifAbsent: [ RPackage named: name ]]