comparing
= other
	^ self species = other species
		and: [self path = other path
			and: [self fileSystem = other fileSystem]]