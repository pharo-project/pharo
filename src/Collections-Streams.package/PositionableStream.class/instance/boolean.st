data get/put
boolean
	"Answer the next boolean value from this (binary) stream."

	^ self next ~= 0
