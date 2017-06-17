visiting
writeVersion: aVersion
	directory := aVersion package name, '.package'.

	self packageFileDirectory deleteAll.
	self writeFormat.
	self writePackage: aVersion package.
	self writeDefinitions: aVersion.
	aVersion dependencies do: [ :ea | self writeVersionDependency: ea ]