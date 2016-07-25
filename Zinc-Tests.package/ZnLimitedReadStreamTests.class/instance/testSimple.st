testing
testSimple
	| data limitedReadStream |
	data := '0123456789'.
	limitedReadStream := ZnLimitedReadStream on: data readStream limit: 5.
	self assert: limitedReadStream upToEnd = '01234'