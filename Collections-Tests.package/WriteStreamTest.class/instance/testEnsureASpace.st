testing
testEnsureASpace
	"self debug: #testEnsureASpace"
	| stream |
	stream := String new writeStream.
	stream nextPutAll: 'this is a test'.
	stream ensureASpace.
	stream nextPutAll: 'for WriteStreamTest'.
	self assert: stream contents = 'this is a test for WriteStreamTest'.
	
	"Manually put a space and verify there are no 2 consecutive spaces"
	stream := String new writeStream.
	stream nextPutAll: 'this is a test '.
	stream ensureASpace.
	stream nextPutAll: 'for WriteStreamTest'.
	self assert: stream contents = 'this is a test for WriteStreamTest'.