tests
testNextPutAllStartingAt
	| ioBuffer |
	ioBuffer := ZdcIOBuffer on: (String new: 12).
	ioBuffer next: 4 putAll: 'abcd' startingAt: 1.
	ioBuffer next: 4 putAll: 'xxxxabcdyyyy' startingAt: 5.
	ioBuffer next: 4 putAll: '--abcd' startingAt: 3.
	self assert: ioBuffer contents equals: 'abcdabcdabcd'