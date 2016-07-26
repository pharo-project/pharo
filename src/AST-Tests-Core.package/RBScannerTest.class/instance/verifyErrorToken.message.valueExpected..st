testing
verifyErrorToken: scannedToken message: message valueExpected: valueExpected
	self assert: scannedToken isError.
	self assert: scannedToken cause equals: message.
	self assert: scannedToken value equals: valueExpected.
	self assert: scannedToken start equals: 1.