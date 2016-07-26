accessing
peek
	"Answer what would be returned if the message next were sent to the 
	receiver. If the receiver is at the end, answer nil."

	| nextObject |
	self atEnd ifTrue: [^nil].
	nextObject := self next.
	position := position - 1.
	^nextObject