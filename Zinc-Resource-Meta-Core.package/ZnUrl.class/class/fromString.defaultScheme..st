instance creation
fromString: string defaultScheme: defaultScheme
	^ (self new)
		parseFrom: string defaultScheme: defaultScheme; 
		yourself 