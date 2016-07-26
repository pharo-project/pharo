data get/put
string
	"Answer the next string from this (binary) stream."

	| size |
	size := self uint16.
	^ (self next: size) asString
