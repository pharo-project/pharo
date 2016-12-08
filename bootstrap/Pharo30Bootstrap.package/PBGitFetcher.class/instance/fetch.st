as yet unclassified
fetch

	| environment |
	self ensureRepository.
	
	environment := Dictionary new.
	(#BaselineOfPharoBootstrap asClass project version allPackagesForSpecNamed: #minimal) do: [ :package | | version |
		version := FiMonticelloWriter new 
			written: (self latestVersionOfPackageNamed: package name) snapshot
			name: package name.
		environment at: package name ifAbsentPut: version.
	].
	
	^ self patchDefinition: environment values asFiSet asRFiEnvironment	