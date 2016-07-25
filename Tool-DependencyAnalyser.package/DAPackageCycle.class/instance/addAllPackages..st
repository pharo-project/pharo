adding
addAllPackages: aCollection
	
	aCollection do: [ :package | self addPackage: package].