tests - printing
testPrintOn

	self assert: (String streamContents: [:stream | false printOn: stream]) = 'false'. 