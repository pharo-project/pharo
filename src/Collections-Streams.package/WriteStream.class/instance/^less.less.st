accessing
<< anObject
	"A more readable, shorter alternative to #nextPutAll: that also
	accepts non-Collection arguments"

	anObject class == collection class
		ifTrue: [ self nextPutAll: anObject ]
		ifFalse: [ anObject putOn: self ]