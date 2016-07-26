accessing
uniqueInstance
	uniqueInstance ifNil: [ self initializeUniqueInstance ].
	^ uniqueInstance