instance creation
entity: anEntity string: aString 
	
	^ self basicNew 
		initializeEnitity: anEntity
		searchString: aString;
		yourself