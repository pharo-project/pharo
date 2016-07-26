accessing
toggleOpenOrClosed
	"toggle the open/closed status of the receiver"
	closed ifTrue: [ self makeOpen ] ifFalse: [ self makeClosed ]