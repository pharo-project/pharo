accessing
uniqueInstance 
	^ uniqueInstance ifNil: [ uniqueInstance  := super new ]