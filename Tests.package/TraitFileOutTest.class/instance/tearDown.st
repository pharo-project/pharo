running
tearDown
	| dir |
	dir := FileSystem workingDirectory.
	self createdClassesAndTraits, self resourceClassesAndTraits  do: [:each |
		(dir / each asString,'st') ensureDelete ] .
	(dir / self categoryName,'st') ensureDelete.
	self class environment organization removeSystemCategory: self categoryName.
	super tearDown