accessing
peek
	"Answer what would be returned if the message next were sent to the receiver. If the receiver is at the end, answer nil.  "
	| next |
	self atEnd ifTrue: [^ nil].
	next := self basicNext.
	self position: self position - 1.
	^ next