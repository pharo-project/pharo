printing
printOn: stream
	super printOn: stream.
	self isEmpty ifFalse: [ self headers printElementsOn: stream ]
	