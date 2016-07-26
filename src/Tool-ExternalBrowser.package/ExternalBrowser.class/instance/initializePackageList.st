initialize
initializePackageList
	packages := self newList.
	self populatePackages.
	packages displayBlock: [ :m | m name ]