testing
testSimpleEof
	| data limitedReadStream |
	data := '0123456789'.
	limitedReadStream := ZnLimitedReadStream on: data readStream limit: 15.
	self assert: limitedReadStream upToEnd = '0123456789'.
	self assert: limitedReadStream atEnd.
	self assert: limitedReadStream next isNil