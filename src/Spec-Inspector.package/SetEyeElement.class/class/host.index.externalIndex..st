instance creation
host: anObject index: index externalIndex: externalIndex
	^ (self host: anObject)
		index: index;
		externalIndex: externalIndex;
		yourself