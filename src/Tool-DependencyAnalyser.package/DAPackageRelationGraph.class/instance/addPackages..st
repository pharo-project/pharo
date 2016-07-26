adding
addPackages: aCollection
	aCollection
		do: [ :package | 
			self addPackage: package ]