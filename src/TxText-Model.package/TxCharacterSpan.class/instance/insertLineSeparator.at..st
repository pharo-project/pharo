operation
insertLineSeparator: separator at: position
	
	| spanPos |
	
	spanPos := position spanPosition.
	
	"at start"
	spanPos = 0 ifTrue: [
		self insertPrev: separator.
		^ position
		].
	
	"at end & middle"
	spanPos < self size ifTrue: [ self splitAt: spanPos ].
	
	self insertNext: separator.
	^ next endPosition
 