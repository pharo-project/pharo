instance creation
fromDiff: aDiff at: anInteger
	^ (self of: aDiff repository)
		fromDiff: aDiff at: anInteger;
		yourself