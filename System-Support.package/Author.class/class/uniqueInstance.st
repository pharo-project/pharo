instance creation
uniqueInstance
	^ uniqueInstance ifNil: [ uniqueInstance := super new ]