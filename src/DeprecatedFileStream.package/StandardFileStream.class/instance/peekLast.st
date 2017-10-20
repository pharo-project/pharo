accessing
peekLast
	"Return that item just put at the end of the stream"

	^ buffer1 size > 0 
		ifTrue: [buffer1 last]
		ifFalse: [nil]
