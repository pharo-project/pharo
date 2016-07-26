accessing
at: aKey ifAbsentPut: aBlock
	^ self at: aKey ifAbsent: [self at: aKey put: aBlock value]