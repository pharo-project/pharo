debugging actions
runToBytecode: aSymbolicBytecode inContext: aContext 
	"Attempt to step over instructions in selectedContext until the 
	execution reaches the selected instruction. This happens when the
	program counter passes the begining of selectionInterval." 
	
	aSymbolicBytecode offset <= aContext pc 
		ifTrue: [ ^self ].
	self stepOver: aContext.
	[ aContext == self interruptedContext and: [ aContext pc < aSymbolicBytecode offset ] ] 
		whileTrue: [ self stepOver: aContext ]