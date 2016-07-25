instance creation
host: anObject label: aLabel description: aBlock value: aValue 
	^ (self host: anObject label: aLabel value: aValue)
		description: aBlock;
		yourself