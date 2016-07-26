testing
nextWordsPutAll: aCollection
	"Write the argument a word-like object in big endian format on the receiver.
	May be used to write other than plain word-like objects (such as ColorArray)."
	
	(aCollection class isPointers or: [ aCollection class isWords not ])
		ifTrue: [ ^ self error: aCollection class name,' is not word-like'].
	1 to: aCollection basicSize do: [ :i |
		self nextNumber: 4 put: (aCollection basicAt: i) ].
	^ aCollection