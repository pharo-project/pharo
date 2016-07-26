marker parsing
perScanSetup
	mcusPerRow := (width / (mcuWidth * DCTSize)) ceiling.
	mcuRowsInScan := (height / (mcuHeight * DCTSize)) ceiling.
	(currentComponents size = 3 or: [ currentComponents size = 1 ]) ifFalse: [ self error: 'JPEG color space not recognized' ].
	mcuMembership := OrderedCollection new.
	currentComponents withIndexDo: 
		[ :c :i | 
		c priorDCValue: 0.
		mcuMembership addAll: ((1 to: c totalMcuBlocks) collect: [ :b | i ]) ].
	mcuMembership := mcuMembership asArray.
	mcuSampleBuffer := (1 to: mcuMembership size) collect: [ :i | IntegerArray new: DCTSize2 ].
	currentComponents withIndexDo: 
		[ :c :i | 
		c initializeSampleStreamBlocks: ((1 to: mcuMembership size) 
				select: [ :j | i = (mcuMembership at: j) ]
				thenCollect: [ :j | mcuSampleBuffer at: j ]) ].
	mcuImageBuffer := Form 
		extent: mcuWidth @ mcuHeight * DCTSize
		depth: 32.
	restartsToGo := restartInterval