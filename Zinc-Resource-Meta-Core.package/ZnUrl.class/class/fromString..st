instance creation
fromString: string
	^ (self new)
		parseFrom: string; 
		yourself 