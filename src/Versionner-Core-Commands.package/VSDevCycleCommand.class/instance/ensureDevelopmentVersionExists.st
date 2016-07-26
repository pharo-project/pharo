checking
ensureDevelopmentVersionExists
	[ workflow developmentVersion ]
		on: MetacelloVersionDoesNotExistError
		do: [ self createDevelopmentVersion ]