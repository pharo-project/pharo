testing
includesAny: aCollection 
	"Answer whether any element of aCollection is one of the receiver's elements."
	aCollection do: [:elem | (self includes: elem) ifTrue: [^ true]].
	^ false