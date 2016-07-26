instance creation
newType: aType size: aNumber
	"Create a new array using internal memory."
	^ self basicNew 
		initializeType: (self resolveType: aType) size: aNumber;
		yourself