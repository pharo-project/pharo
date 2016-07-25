printing
printMethodAndUriOn: stream
	(self method notNil and: [ self uri notNil ])
		ifFalse: [ ^ self ].
	stream
		nextPutAll: self method;
		space.
	self uri printPathQueryFragmentOn: stream