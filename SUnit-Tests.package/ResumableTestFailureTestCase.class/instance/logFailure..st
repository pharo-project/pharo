logging
logFailure: aString
	duplicateFailureLog add: aString.
	super logFailure: aString.