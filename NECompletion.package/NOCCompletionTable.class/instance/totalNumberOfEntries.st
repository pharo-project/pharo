accessing
totalNumberOfEntries
	^ table inject: 0 into: [:total :list | total + list numEntries]