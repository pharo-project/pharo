accessing
text: anObject
	text := anObject ifNil: [ self defaultText ]