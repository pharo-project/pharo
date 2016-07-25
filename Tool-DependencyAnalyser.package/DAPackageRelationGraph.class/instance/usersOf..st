computing
usersOf: aPackage
	| users |
	users := OrderedCollection new.
	self seenPackages
		do: [ :package | 
			(package dependentPackages includes: aPackage)
				ifTrue: [ users add: package ] ].
	^ users