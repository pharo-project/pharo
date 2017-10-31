tests - compact
testCompactAtEnd
	| ioBuffer |
	ioBuffer := ZdcIOBuffer on: (ByteArray new: 10).
	10 timesRepeat: [ ioBuffer nextPut: 1 ].
	self assert: ioBuffer gapAtFront isZero.
	10 timesRepeat: [ ioBuffer next ].
	self assert: ioBuffer isFull.
	self assert: ioBuffer gapAtFront = 10.
	ioBuffer compact.
	self assert: ioBuffer isEmpty
	
		