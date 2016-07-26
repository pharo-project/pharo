testing
testBinaryAll
	| data limitedReadStream |
	data := #(0 1 2 3 4 5 6 7 8 9) asByteArray.
	limitedReadStream := ZnLimitedReadStream on: data readStream limit: data size.
	self assert: limitedReadStream upToEnd = data