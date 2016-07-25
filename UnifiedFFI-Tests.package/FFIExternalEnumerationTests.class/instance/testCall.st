tests
testCall
	| result buffer |

	buffer := ByteArray new: 100. 
	result := self ffiTestCall: self enumClass DDD format: '%d' to: buffer.
	self assert: result equals: 4.
	self assert: (buffer first: result) asString equals: '2400'	