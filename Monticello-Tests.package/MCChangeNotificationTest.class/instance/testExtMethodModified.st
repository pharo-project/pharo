tests
testExtMethodModified
	| event mref |
	workingCopy modified: false.
	mref := workingCopy packageSet extensionMethods first.
	event := self modifiedEventFor: mref selector ofClass: mref actualClass.
	MCWorkingCopy methodModified: event.
	self assert: workingCopy modified