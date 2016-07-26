tests
testCoreMethodModified
	| event |
	workingCopy modified: false.
	event := self modifiedEventFor: #one ofClass: self mockClassA.
	MCWorkingCopy methodModified: event.
	self assert: workingCopy modified