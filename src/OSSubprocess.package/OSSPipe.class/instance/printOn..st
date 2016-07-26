printing
printOn: aStream
	"The implementation of Stream>>printOn: has bad side effects when used
	for OSPipe. This implementation is copied from Object."

	| title |
	title := self class name.
	aStream
		nextPutAll: (title first isVowel ifTrue: ['an '] ifFalse: ['a ']);
		nextPutAll: title