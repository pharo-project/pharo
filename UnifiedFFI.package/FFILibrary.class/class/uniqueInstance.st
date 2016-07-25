instance creation
uniqueInstance 
	self = FFILibrary 
		ifTrue: [ self error: 'I''m an abstract class, use one of my children.' ].
	^ uniqueInstance ifNil: [ uniqueInstance := super new ]