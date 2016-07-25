testing
testNextCount
	| data stream |
	data := '0123456789'.
	stream := ZnLimitedReadStream on: data readStream limit: 8.
	self assert: (stream next: 4) = '0123'