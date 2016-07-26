testing
testAdvanceReadPointer
	| ioBuffer offset |
	ioBuffer := ZdcIOBuffer on: (ByteArray new: 10).
	3 timesRepeat: [ ioBuffer nextPut: 0 ].
	1 to: 6 do: [ :each | ioBuffer nextPut: each ].
	3 timesRepeat: [ ioBuffer next ].
	offset := ioBuffer contentsStart.
	self assert: ioBuffer availableForReading = 6.
	1 to: 6 do: [ :each | 
		self assert: (ioBuffer buffer at: offset + each - 1) = each ].
	ioBuffer advanceReadPointer: 6.
	self assert: ioBuffer availableForReading = 0.
	ioBuffer compact.
	self assert: ioBuffer isEmpty     
	