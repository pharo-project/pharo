accessing
keyAtValue: value 
	"Answer the key that is the external name for the argument, value. If 
	there is none, signal an error."

	^self keyAtValue: value ifAbsent: [self errorValueNotFound: value]