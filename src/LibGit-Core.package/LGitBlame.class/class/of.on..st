instance creation
of: aRepository on: aFileReference
	^ (self of: aRepository)
		getForReference: aFileReference;
		yourself