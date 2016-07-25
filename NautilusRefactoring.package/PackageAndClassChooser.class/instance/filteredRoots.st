accessing - tree
filteredRoots
	|  roots |
	roots := RPackageOrganizer default packages 
		select: [ :package | searchPattern search: package name ].
	roots := roots asSortedCollection: [ :a :b | a name <= b name ].
	Smalltalk allClassesAndTraitsDo: [ :class | 
		(searchPattern search: class name)
			ifTrue: [ roots add: class ]].
	^ roots