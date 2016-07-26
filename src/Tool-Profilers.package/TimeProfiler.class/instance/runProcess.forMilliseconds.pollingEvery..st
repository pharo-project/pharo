actions
runProcess: aProcess forMilliseconds: msecDuration pollingEvery: pollPeriod 
	block := MessageSend 
				receiver: self
				selector: #runProcess:forMilliseconds:pollingEvery: 
				arguments: { 
						aProcess.
						msecDuration.
						pollPeriod}.	"so we can re-run it"
	rootMessageTally := MessageTally new.
	rootMessageTally
		reportOtherProcesses: false;
		maxClassNameSize: 1000;
		maxClassPlusSelectorSize: 1000;
		maxTabs: 100.
	result := rootMessageTally 
				spyEvery: pollPeriod
				onProcess: aProcess
				forMilliseconds: msecDuration.
	^result