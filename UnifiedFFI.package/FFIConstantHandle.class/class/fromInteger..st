instance creation
fromInteger: anIntegerValue
	anIntegerValue ifNil: [ self error: 'Invalid value for ' , self name , ' contant handle' ].
	^ self new 
		setHandle: anIntegerValue;
		yourself