tests
testPrintOn
	self assert: (String streamContents: [ :stream | delivery printOn: stream ]) notEmpty.