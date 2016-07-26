tests
testReturn
	| result buffer |

	buffer := ByteArray new: 100. 
	result := self ffiTestReturn: 123 format: '%d' to: buffer.
	self assert: result equals: self enumClass CCC.
	self assert: (buffer first: result value) asString equals: '123'	