instance creation
externalNewType: aType size: aNumber
	"Create a new array in the C heap. 
	 This array needs to be disposed (using #free method)"
	^ self basicNew 
		initializeExternalType: (self resolveType: aType) size: aNumber;
		yourself