converting
asArray
	"Answer an Array whose elements are the elements of the receiver.
	Implementation note: Cannot use ''Array withAll: self'' as that only
	works for SequenceableCollections which support the replacement 
	primitive."

	| newArray index |
	newArray := Array new: self size.
	index := 0.
	self do: [:each | newArray at: (index := index + 1) put: each].
	^ newArray