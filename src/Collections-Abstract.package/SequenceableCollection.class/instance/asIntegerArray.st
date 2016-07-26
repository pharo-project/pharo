converting
asIntegerArray
	"Answer an IntegerArray whose elements are the elements of the receiver, in 
	the same order."

	| intArray |
	intArray := IntegerArray new: self size.
	1 to: self size do:[:i| intArray at: i put: (self at: i)].
	^intArray