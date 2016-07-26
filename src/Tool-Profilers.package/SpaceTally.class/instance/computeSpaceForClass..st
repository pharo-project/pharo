fileout
computeSpaceForClass: cl
	| codeSpace totalInstSpace instSpace totalInstCount totalCodeSpace instCount |
	codeSpace := cl spaceUsed.
	Smalltalk garbageCollectMost.
	instCount := cl instanceCount.
	instSpace := cl instancesSizeInMemory.
	totalCodeSpace := totalCodeSpace + codeSpace.
	totalInstCount := totalInstCount + instCount.
	totalInstSpace := totalInstSpace + instSpace.
	^ results
		add:
			(SpaceTallyItem
				analyzedClassName: cl name
				codeSize: codeSpace
				instanceCount: instCount
				spaceForInstances: instSpace)