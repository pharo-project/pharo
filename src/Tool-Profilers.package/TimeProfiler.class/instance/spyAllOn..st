actions
spyAllOn: aBlock 
	block := aBlock.
	rootMessageTally := MessageTally new.
	rootMessageTally
		reportOtherProcesses: true;
		maxClassNameSize: 1000;
		maxClassPlusSelectorSize: 1000;
		maxTabs: 100.
	result := rootMessageTally spyAllEvery: MessageTally defaultPollPeriod on: aBlock.
	self result: result.
	^ result
