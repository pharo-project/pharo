data get/put
boolean: aBoolean
	"Store the given boolean value on this (binary) stream."

	self nextPut: (aBoolean ifTrue: [1] ifFalse: [0]).
