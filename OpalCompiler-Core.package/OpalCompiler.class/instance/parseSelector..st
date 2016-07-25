public access
parseSelector: aString 
	"Answer the message selector for the argument, aString, which should parse successfully up to the temporary declaration or the end of the method header."
	
	^[RBParser parseMethodPattern: aString] on: Error do: [nil].