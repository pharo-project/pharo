code generation (closures)
noteBlockExit: aBlock
	"Evaluate aBlock with the numbering for the block exit."
	aBlock value: locationCounter + 1.
	locationCounter := locationCounter + 2