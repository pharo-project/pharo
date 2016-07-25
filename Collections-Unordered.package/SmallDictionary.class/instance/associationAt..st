accessing - associations
associationAt: key 
	^ self associationAt: key ifAbsent: [self errorKeyNotFound: key]