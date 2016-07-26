testing
isAvailable
	"This is (and must be) a lazy method.  If my current has a value, an attempt to make me available has already been made:  trust its result.  If not, try to make me available."

	current ifNil: [self makeAvailable].
	^self isAlreadyAvailable