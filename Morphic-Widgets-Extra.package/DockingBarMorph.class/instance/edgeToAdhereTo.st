private - accessing
edgeToAdhereTo
	"private - answer the edge where the receiver is adhering to"
	^ self
		valueOfProperty: #edgeToAdhereTo
		ifAbsent: [#none]