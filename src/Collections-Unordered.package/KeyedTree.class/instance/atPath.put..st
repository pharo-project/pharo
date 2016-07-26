accessing
atPath: anArray put: aBlock
	"Answer the value of aBlock after creating its path."

	|element|
	anArray isEmpty
		ifTrue: [^self].
	element := self.
	anArray allButLastDo: [:key |
		element := element at: key ifAbsentPut: [self species new]].
	^element at: anArray last put: aBlock