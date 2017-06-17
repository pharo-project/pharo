tests
testEnsureEndsWith
	"self debug: #testEnsureEndsWith"
	| stream |
	stream := self newStream.
	stream nextPutAll: 'this is a test'.
	stream ensureEndsWith: Character cr.
	stream nextPutAll: 'for WriteStreamTest'.
	self assert: stream contents equals: (('this is a test' copyWith: Character cr), 'for WriteStreamTest').
	
	"Manually put a new line and verify there are no 2 new lines"
	stream := self newStream.
	stream nextPutAll: ('this is a test' copyWith: Character cr).
	stream ensureEndsWith: Character cr.
	stream nextPutAll: 'for WriteStreamTest'.
	self assert: stream contents equals: (('this is a test' copyWith: Character cr), 'for WriteStreamTest').
	
	"Test with a empty stream"
	stream := self newStream.
	stream ensureEndsWith: Character cr.
	self assert: stream contents equals: ''.