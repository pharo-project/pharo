testing
testBinary
	| data limitedReadStream |
	data := #(0 1 2 3 4 5 6 7 8 9) asByteArray.
	limitedReadStream := ZnLimitedReadStream on: data readStream limit: 5.
	self assert: limitedReadStream upToEnd = #(0 1 2 3 4) asByteArray