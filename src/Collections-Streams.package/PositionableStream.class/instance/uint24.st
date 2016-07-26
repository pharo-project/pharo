data get/put
uint24
	"Answer the next unsigned, 24-bit integer from this (binary) stream."

	| n |
	n := self next.
	n := (n bitShift: 8) + self next.
	n := (n bitShift: 8) + self next.
	^ n
