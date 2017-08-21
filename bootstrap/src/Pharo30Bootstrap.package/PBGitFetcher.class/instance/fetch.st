as yet unclassified
fetch

	| environment |
	self ensureRepository.
	
	environment := Dictionary new.
	#BaselineOfPharoBootstrap asClass kernelPackageNames do: [ :package | | version |
		version := FiMonticelloWriter new 
			written: (self latestVersionOfPackageNamed: package) snapshot
			name: package.
		environment at: package ifAbsentPut: version.
	].
	
	^ environment values asFiSet asRFiEnvironment	