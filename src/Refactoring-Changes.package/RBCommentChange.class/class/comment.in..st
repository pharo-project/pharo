instance creation
comment: aString in: aClass
	^ self new
		changeClass: aClass;
		comment: aString;
		yourself