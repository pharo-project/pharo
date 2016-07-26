peeking
peek
	"Answer what would be returned if the message next were sent to the receiver. If the receiver is at the end, answer nil.  "
	self atEnd ifTrue: [^ nil ].

	peekBuffer ifNotNil: [ ^ peekBuffer ].

	^ peekBuffer := self next.