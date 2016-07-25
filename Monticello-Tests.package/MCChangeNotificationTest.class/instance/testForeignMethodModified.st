tests
testForeignMethodModified
	| event |
	workingCopy modified: false.
	event := self modifiedEventFor: #foreignMethod ofClass: self class.
	MCWorkingCopy methodModified: event.
	self deny: workingCopy modified