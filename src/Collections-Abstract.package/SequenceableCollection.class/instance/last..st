accessing
last: n
	"Answer the last n elements of the receiver.  
	Raise an error if there are not enough elements."

	| size |
	size := self size.
	^ self copyFrom: size - n + 1 to: size