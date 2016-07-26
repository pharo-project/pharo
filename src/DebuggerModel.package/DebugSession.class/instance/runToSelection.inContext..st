debugging actions
runToSelection: selectionInterval inContext: aContext 
	"Attempt to step over instructions in selectedContext until the 
	execution reaches the selected instruction. This happens when the
	program counter passes the begining of selectionInterval.
	
	A not nill and valid interval is expected." 
	
	(self pcRangeForContext: aContext) first >= selectionInterval first 
		ifTrue: [ ^self ].
	self stepOver: aContext.
	[ aContext == self interruptedContext and: [ (self pcRangeForContext: aContext) first < selectionInterval first ] ] 
		whileTrue: [ self stepOver: aContext ]