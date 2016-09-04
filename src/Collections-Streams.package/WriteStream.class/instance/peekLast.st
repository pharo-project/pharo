accessing
peekLast
	"Return that item just put at the end of the stream"

	^ position > 0 
		ifTrue: [collection at: position]
		ifFalse: [nil]