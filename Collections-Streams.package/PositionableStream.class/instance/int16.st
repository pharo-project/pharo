data get/put
int16
	"Answer the next signed, 16-bit integer from this (binary) stream."

	| n |
	n := self next.
	n := (n bitShift: 8) + (self next).
	n >= 16r8000 ifTrue: [n := n - 16r10000].
	^ n
