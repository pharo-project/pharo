marker parsing
parseFirstMarker
	| marker |
	self next = 255 ifFalse: [ self error: 'JFIF marker expected' ].
	marker := self next.
	marker = 217 ifTrue: [ ^ self	"halt: 'EOI encountered.'" ].
	marker = 216 ifFalse: [ self error: 'SOI marker expected' ].
	self parseStartOfInput