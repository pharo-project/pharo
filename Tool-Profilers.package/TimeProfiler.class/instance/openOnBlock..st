public opening
openOnBlock: aBlock 
	self runBlock: aBlock pollingEvery: MessageTally defaultPollPeriod.
	self open.
	^ self