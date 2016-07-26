read, write, position
upToEnd
	"Answer the remaining elements in the string."

	| strm s |
	strm := WriteStream on: String new.
	[(s := self next: 2000) isEmpty
		ifTrue: [^ strm contents]
		ifFalse: [strm nextPutAll: s]] repeat
