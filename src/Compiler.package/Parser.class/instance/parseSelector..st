public access
parseSelector: aString 
	"Answer the message selector for the argument, aString, which should 
	 parse successfully up to the temporary declaration or the end of the 
	 method header."

	self initScannerForTokenization.
	^self
		initPattern: aString
		notifying: nil
		return: [:pattern | pattern at: 1]