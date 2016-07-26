instance creation
new 
	^ self basicNew 
		initializeType: self type size: self numberOfElements;
		yourself