enumerating
segmentsDo: aBlock
	"Evaluate the two-argument block with each vertex and its successor."

	self vertices size < 2 ifTrue: [^self].
	1 to: self vertices size - 1 do: [:i |
		aBlock
			value: (self vertices at: i)
			value: (self vertices at: i + 1)]