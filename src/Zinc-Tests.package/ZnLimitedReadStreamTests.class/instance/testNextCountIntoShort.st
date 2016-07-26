testing
testNextCountIntoShort
	| data stream buffer |
	data := '0123456789'.
	stream := ZnLimitedReadStream on: data readStream limit: 5.
	buffer := String new: 6.
	self assert: (stream next: 6 into: buffer) = '01234'
	