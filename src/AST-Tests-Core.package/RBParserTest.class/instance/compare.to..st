accessing
compare: anObject to: anotherObject 
	self assert: anObject hash = anotherObject hash.
	self assert: anObject = anotherObject