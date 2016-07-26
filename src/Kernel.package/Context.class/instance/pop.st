controlling
pop
	"Answer the top of the receiver's stack and remove the top of the stack."
	| value |
	value := self at: stackp.
	self stackp: stackp - 1.
	^ value