converting
asFloatArray
	"Answer a FloatArray whose elements are the elements of the receiver, in 
	the same order."

	| floatArray |
	floatArray := FloatArray new: self size.
	1 to: self size do:[:i| floatArray at: i put: (self at: i) asFloat ].
	^floatArray