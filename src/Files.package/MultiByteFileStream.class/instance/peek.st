accessing
peek
	"Answer what would be returned if the message next were sent to the receiver. If the receiver is at the end, answer nil.  "

	| next save |
	self atEnd ifTrue: [^ nil].
	save := converter saveStateOf: self.
	next := self next.
	converter restoreStateOf: self with: save.
	^ next.

