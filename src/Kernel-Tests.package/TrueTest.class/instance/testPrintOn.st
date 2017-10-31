tests - printing
testPrintOn

	self assert: (String streamContents: [:stream | true printOn: stream]) = 'true'. 