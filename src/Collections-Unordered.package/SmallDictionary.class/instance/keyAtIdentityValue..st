accessing
keyAtIdentityValue: value 
	"Answer the key that is the external name for the argument, value. If 
	there is none, answer nil.
	Note: There can be multiple keys with the same value. Only one is returned."

	^self keyAtIdentityValue: value ifAbsent: [self errorValueNotFound: value]