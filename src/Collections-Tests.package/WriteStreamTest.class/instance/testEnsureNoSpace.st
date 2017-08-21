tests
testEnsureNoSpace
	"self debug: #testEnsureASpace2"
	| stream |
	stream := self newStream.
	stream ensureNoSpace.
	
	self assert: stream contents equals: ''.
	
	stream nextPutAll: ' a '.
	stream ensureNoSpace.
	self assert: stream contents equals: ' a'.
	
	stream nextPutAll: 'b  '.
	stream ensureNoSpace.
	self assert: stream contents equals: ' ab'.

	
	
	