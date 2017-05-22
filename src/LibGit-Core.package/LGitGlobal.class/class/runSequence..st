operations
runSequence: aBlock
	LGitActionSequence value ifNotNil: aBlock.
	
	LGitActionSequence
		value: thisContext sender
		during: [
			self checkInitialized.
			aBlock value ]