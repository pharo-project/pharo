instance creation
on: object
	^ (self basicNew)
		initialize;
		on: object;
		yourself