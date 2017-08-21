generating
generateOn: aStream
	aStream nextPutAll: '<?xml version="1.0" encoding="UTF-8"?>'; nextPut: Character lf.
	aStream nextPutAll: '<checkstyle version="5.2">'; nextPut: Character lf.
	(environment allClasses asSortedCollection: [ :a :b | a name <= b name ])
		do: [ :class | self generateClass: class on: aStream ].
	aStream nextPutAll: '</checkstyle>'