accessing
atPath: anArray ifAbsentPut: aBlock
	"Answer the element referenced by the given key path.
	Answer the value of aBlock if not found after creating its path."

	|element|
	anArray isEmpty
		ifTrue: [^self].
	element := self.
	anArray allButLastDo: [:key |
		element := element at: key ifAbsentPut: [self species new]].
	^element at: anArray last ifAbsentPut: aBlock