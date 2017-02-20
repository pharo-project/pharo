instance creation
hostObject: anObject label: aString value: anotherObject
	^ (self hostObject: anObject) 
		label: aString;
		value: anotherObject;
		yourself