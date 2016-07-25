public
spyOn: aBlock toFileNamed: fileName reportOtherProcesses: aBoolean
	"Spy on the evaluation of aBlock. Write the data collected on a file
	named fileName."

	| file value node |
	node := self new.
	node reportOtherProcesses: aBoolean.
	value := node spyEvery: self defaultPollPeriod on: aBlock.
	file := FileStream newFileNamed: fileName.
	node report: file; close.
	file close.
	^value