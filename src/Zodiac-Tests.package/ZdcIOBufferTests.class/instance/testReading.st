testing
testReading
	| ioBuffer |
	ioBuffer := ZdcIOBuffer on: (ByteArray new: 10).
	1 to: 6 do: [ :each | ioBuffer nextPut: each ].
	1 to: 3 do: [ :each | self assert: ioBuffer next = each ].
	self assert: ioBuffer contents = #[4 5 6].
	4 to: 6 do: [ :each | self assert: ioBuffer next = each ].
	self assert: ioBuffer isEmpty.
	self should: [ ioBuffer next ] raise: Error