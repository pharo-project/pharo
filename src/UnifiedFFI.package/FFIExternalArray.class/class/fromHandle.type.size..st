instance creation
fromHandle: aHandle type: aType size: aNumber
	"aHandle is the address to an array already existing. 
	 For example, C: 
	 	
		byte[]	 aHandle;
			
	 which means is a direct reference and array starts in same address as aHandle"
	^ self basicNew 
		initializeHandle: aHandle type: (self resolveType: aType) size: aNumber;
		yourself