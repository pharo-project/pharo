testing
testNextCountShort
	| data stream |
	data := '0123456789'.
	stream := ZnLimitedReadStream on: data readStream limit: 5.
	self assert: (stream next: 6) = '01234'