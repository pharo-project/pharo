testing
testNextCountInto
	| data stream buffer |
	data := '0123456789'.
	stream := ZnLimitedReadStream on: data readStream limit: 8.
	buffer := String new: 4.
	self assert: (stream next: 4 into: buffer) = '0123'.
	self assert: buffer = '0123'