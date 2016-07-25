cleaning
addRepositoryToPackagesWithConfiguration
	"Add repositories from configuration to packages included in one. 
	 Answers the complete list of working copies with repository setted."
	| workingCopies versions |
	
	workingCopies := #().
	
	versions := Smalltalk allClasses 
		select: [ :each | (each name beginsWith: 'ConfigurationOf') and: [ each ~= ConfigurationOf ] ]
		thenCollect: [ :each | 
			[ each project version: #stable ]
			on: MetacelloSymbolicVersionDoesNotExistError
			do: [ :e | 
				"no stable version, trying bleedingEdge" 
				each project version: #bleedingEdge ] ].
	
	versions do: [ :eachVersion | 
		| projectWorkingCopies repositories |
		"Collect repositories (ideally just one)"
		repositories := eachVersion spec repositories 
			collect: #createRepository.
		"Add to default"
		repositories do: [ :eachRepository | eachRepository addTo: MCRepositoryGroup default ].
		"Add to working copies"
		projectWorkingCopies :=
			({eachVersion project configuration class package mcWorkingCopy}, 
		 	 (eachVersion packages collect: #workingCopy thenSelect: #notNil)).
		projectWorkingCopies do: [ :eachWorkingCopy | 
			repositories do: [ :eachRepository | 
				eachRepository addTo: eachWorkingCopy repositoryGroup ] ].
		workingCopies := workingCopies, projectWorkingCopies ].

	^ workingCopies