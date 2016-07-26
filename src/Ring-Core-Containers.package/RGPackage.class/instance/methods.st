accessing
methods
	"Retrieves all the methods defined in the receiver. 
	#methods holds the methods of defined classes"
	
	^super methods, self extensionMethods 