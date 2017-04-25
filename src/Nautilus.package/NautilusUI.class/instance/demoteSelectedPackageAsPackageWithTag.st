menu-packages
demoteSelectedPackageAsPackageWithTag

	| packages |
	packages := self selectedPackages.
	
	packages 
		select: [ :each | each name includes: $- ]
		thenDo: [ :each | each demoteToRPackageNamed: (each name copyUpToLast: $-) ]

