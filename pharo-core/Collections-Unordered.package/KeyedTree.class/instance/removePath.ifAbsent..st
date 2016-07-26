removing
removePath: anArray ifAbsent: aBlock
	"Remove and answer the element referenced by the given path.
	Answer the value of aBlock if not found."

	|element|
	anArray isEmpty
		ifTrue: [^self].
	element := self.
	anArray allButLastDo: [:key |
		element := element at: key ifAbsent: [^aBlock value]].
	^element removeKey: anArray last ifAbsent: aBlock	