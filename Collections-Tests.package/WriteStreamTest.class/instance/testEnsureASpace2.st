testing
testEnsureASpace2
	"self debug: #testEnsureASpace2"
	| stream |
	stream := String new writeStream.
	stream ensureASpace.
	self assert: stream contents = ' '.
	