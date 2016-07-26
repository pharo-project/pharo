instance creation
dataSize "^<Integer>"
	"Answer the number of bytes required to represent an instance of the receiver.
	This excludes the size word's own bytes and is just the size of the data."
	^self subclassResponsibility