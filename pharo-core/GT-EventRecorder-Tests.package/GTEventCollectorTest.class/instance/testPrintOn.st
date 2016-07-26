tests
testPrintOn
	self assert: (String streamContents: [ :stream | collector printOn: stream ]) notEmpty.