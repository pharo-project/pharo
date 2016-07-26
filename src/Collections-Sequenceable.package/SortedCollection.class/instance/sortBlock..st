accessing
sortBlock: aBlock 
	"Make the argument, aBlock, be the criterion for ordering elements of the 
	receiver."

	sortBlock := aBlock.
	"sortBlocks with side effects may not work right"
	self size > 0 ifTrue: [self reSort]