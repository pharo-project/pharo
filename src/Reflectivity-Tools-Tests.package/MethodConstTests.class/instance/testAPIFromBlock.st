tests
testAPIFromBlock

	| constInitialResult constSecondResult |
	constInitialResult := self constFromBlock: [DateAndTime now].
	constSecondResult := self constFromBlock: nil.
	
	self assert: constSecondResult == constInitialResult