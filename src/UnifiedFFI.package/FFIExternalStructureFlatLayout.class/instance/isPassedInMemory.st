testing
isPassedInMemory
	^ fields size = 1 and: [ fields first registerClass == #memory ]