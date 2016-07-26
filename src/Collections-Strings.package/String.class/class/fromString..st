instance creation
fromString: aString 
	"Answer an instance of me that is a copy of the argument, aString."
	
	^ aString copyFrom: 1 to: aString size