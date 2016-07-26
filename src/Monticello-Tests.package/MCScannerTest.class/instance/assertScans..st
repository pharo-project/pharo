asserting
assertScans: anArray
	self assert: (MCScanner scan: anArray printString readStream) = anArray