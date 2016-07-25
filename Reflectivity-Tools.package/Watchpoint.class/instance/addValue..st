adding
addValue: anObject
	values add: 
		(DateAndTime now -> anObject).
	^anObject.
