instance creation
key: newKey value: newValue
	"Answer an instance of me with the arguments as the key and value of 
	the association."

	^(self basicNew: 1) key: newKey value: newValue