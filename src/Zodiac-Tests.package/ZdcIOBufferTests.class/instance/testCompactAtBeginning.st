tests - compact
testCompactAtBeginning
	| ioBuffer |
	ioBuffer := ZdcIOBuffer on: (ByteArray new: 10).
	ioBuffer compact.
	self assert: ioBuffer isEmpty.
	self assert: ioBuffer availableForWriting = 10.
	1 to: 6 do: [ :each | ioBuffer nextPut: each ].
	ioBuffer compact.
	self assert: ioBuffer availableForReading = 6.
	self assert: ioBuffer availableForWriting = 4.
	self assert: ioBuffer gapAtFront isZero