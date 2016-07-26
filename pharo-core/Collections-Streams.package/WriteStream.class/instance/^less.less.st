printing
<< aCollection
	"we want a readable version of nextPutAll, that also works with non Collection arguments"

	collection class == aCollection class ifFalse:
		[ aCollection putOn: self. ^ self ].

	self nextPutAll: aCollection
