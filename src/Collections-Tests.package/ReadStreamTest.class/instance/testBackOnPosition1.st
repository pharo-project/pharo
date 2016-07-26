tests
testBackOnPosition1
	"Test the new implementation of the method back."
	|stream|
	stream := 'abc' readStream.
	stream next.
	self assert: stream back = $a.
