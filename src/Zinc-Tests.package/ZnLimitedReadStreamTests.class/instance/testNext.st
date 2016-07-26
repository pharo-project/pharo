testing
testNext
	| limitedReadStream |
	limitedReadStream := ZnLimitedReadStream on: String new readStream limit: 10.
	self assert: limitedReadStream atEnd.
	self assert: limitedReadStream next isNil.
	limitedReadStream := ZnLimitedReadStream on: 'ABC' readStream limit: 10.
	self assert: limitedReadStream atEnd not.
	self assert: limitedReadStream next = $A.
	self assert: limitedReadStream peek = $B.
	limitedReadStream next.
	self assert: limitedReadStream next = $C.
	self assert: limitedReadStream next isNil.
	self assert: limitedReadStream atEnd
	