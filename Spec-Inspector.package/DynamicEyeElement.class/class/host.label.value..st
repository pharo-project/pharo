instance creation
host: anObject label: aLabel value: aValue
	^ (self host: anObject)
		label: aLabel;
		value: aValue;
		yourself