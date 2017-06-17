instance creation
of: aRepository fromId: id
	^ (self of: aRepository)
		initializeWithId: id;
		yourself