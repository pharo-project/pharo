spying
spyOnProcess: aProcess forMilliseconds: msecDuration toFileNamed: fileName reportOtherProcesses: aBoolean
	"Spy on the evaluation of aProcess. Write the data collected on a file  
	named fileName. Will overwrite fileName"
	| file node |
	node := self new.
	node reportOtherProcesses: aBoolean.
	node
		spyEvery: self defaultPollPeriod
		onProcess: aProcess
		forMilliseconds: msecDuration.
	file := FileStream fileNamed: fileName.
	node report: file.
	file close