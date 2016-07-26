instance creation
new
	"Answer a new initialized instance of the receiver (which is a class) with no indexable variables. Fail if the class is indexable."

	^ self basicNew initialize
