actions
runBlock: aBlock pollingEvery: pollPeriod 
	block := aBlock.
	rootMessageTally := MessageTally new.
	rootMessageTally
		reportOtherProcesses: true;
		maxClassNameSize: 1000;
		maxClassPlusSelectorSize: 1000;
		maxTabs: 100.
	result := rootMessageTally spyEvery: pollPeriod on: aBlock.
	self result: result.
	^ result
