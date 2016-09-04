tests
testEnsureASpace2
	"self debug: #testEnsureASpace2"
	| stream |
	stream := self newStream.
	stream ensureASpace.
	self assert: stream contents = ' '.
	