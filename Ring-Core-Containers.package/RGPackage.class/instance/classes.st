accessing
classes
	"Retrieves a collection (by default a dictionary) of classes defined in the receiver and classes holding extension methods"
	
	^self definedClasses, self extendedClasses 