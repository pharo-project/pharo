instance creation
managerFor: aRepository
	^ self managers 
		at: aRepository 
		ifAbsentPut: [ self basicNew initializeRepository: aRepository ].
