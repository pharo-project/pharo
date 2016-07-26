private - actions
feedback: aString

	self silent 
		ifFalse: [self inform: aString]
		ifTrue: [Transcript show: aString ; cr]