tests
testPeekBack
	"Test the new implementation of the method peekBack due to changing #back."
	|stream|
	stream := 'abc' readStream.
	stream setToEnd.
	self assert: stream peekBack = $c.
