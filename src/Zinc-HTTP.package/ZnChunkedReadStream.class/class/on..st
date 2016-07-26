instance creation
on: stream
	^ self basicNew
		initialize;
		on: stream;
		yourself