enumerating
segmentsDo: aBlock
	"Evaluate the two-argument block with each vertex and its successor."

	self vertices size < 2 ifTrue: [^self].
	super segmentsDo: aBlock.
	aBlock
		value: self vertices last
		value: self vertices first