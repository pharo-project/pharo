converting
asWordArray
	"Answer a WordArray whose elements are the elements of the receiver, in 
	the same order."

	| wordArray |
	wordArray := WordArray new: self size.
	1 to: self size do:[:i| wordArray at: i put: (self at: i)].
	^wordArray