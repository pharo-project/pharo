accessing
atPath: anArray ifAbsent: aBlock
	"Answer the element referenced by the given key path.
	Answer the value of aBlock if not found."

	|element|
	element := self.
	anArray do: [:key |
		element := element at: key ifAbsent: [^aBlock value]].
	^element