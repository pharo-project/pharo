instance creation
externalNew 
	"Create a new array in the C heap. 
	 This array needs to be disposed (using #free method)... or you can declare it #autoRelease"
	^ self basicNew 
		initializeExternalType: self type size: self numberOfElements;
		yourself