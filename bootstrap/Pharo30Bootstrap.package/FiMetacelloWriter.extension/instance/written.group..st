*Pharo30Bootstrap
written: aMetacelloVersion group: aGroup
	
	| environment record  |
	environment := Dictionary new.
	record := self recordVersion: aMetacelloVersion group: aGroup.
	
	record loadDirective packageDirectivesDo: [:directive | | repository package |
		repository := (record repositoryMap 
			at: directive externalReference packageName
			ifAbsent: [ record repositoryMap 
										at: directive externalReference name ]).
		package := self loadPackage: directive file fromRepository: repository.
		environment at: package name ifAbsentPut: package.
	].
	
	^ environment values asFiSet asRFiEnvironment