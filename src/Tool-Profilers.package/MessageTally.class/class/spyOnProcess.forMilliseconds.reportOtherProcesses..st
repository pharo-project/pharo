spying
spyOnProcess: aProcess forMilliseconds: msecDuration reportOtherProcesses: aBoolean
	"
	Spy on aProcess for a certain amount of time
	| p1 p2 |  
	p1 := [100000 timesRepeat: [3.14159 printString. Processor yield]] newProcess.  
	p2 := [100000 timesRepeat: [3.14159 printString. Processor yield]] newProcess.
	p1 resume.
	p2 resume.  
	(Delay forMilliseconds: 100) wait.  
	MessageTally spyOnProcess: p1 forMilliseconds: 1000 reportOtherProcesses: true
	"
	| node |
	node := self new.
	node reportOtherProcesses: aBoolean.
	node
		spyEvery: self defaultPollPeriod
		onProcess: aProcess
		forMilliseconds: msecDuration.
	StandardWindow new
		withText:  (String streamContents: [:s | node report: s])
		 label: 'Spy Results';
		openInWorld.