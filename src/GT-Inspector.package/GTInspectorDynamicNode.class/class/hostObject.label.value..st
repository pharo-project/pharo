instance creation
hostObject: anObject label: aString value: anotherObject
	^ (self hostObject: anObject) 
		label: aString;
		rawValue: anotherObject;
		yourself