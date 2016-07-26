tests
testPrintOn
	self assert: (String streamContents: [ :stream | collectors printOn: stream ]) notEmpty.