accessing
key: aKey value: anObject 
	"Store the arguments as the variables of the receiver."

	self basicAt: 1 put: aKey.
	value := anObject