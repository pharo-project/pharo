tests
testBack
	|stream|
	stream := 'abc' readStream.
	stream setToEnd.
	self assert: stream back = $c.
