accessing - keys
keyAtValue: value 
	"Answer the key that is the external name for the argument, value. If 
	there is none, answer nil."

	^self keyAtValue: value ifAbsent: [self errorValueNotFound: value]