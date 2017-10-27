context
pcRangeForContext: aContext
	"Answer the indices in the source code for the method corresponding to 
	aContext's program counter value."
	(aContext isNil) ifTrue: [ 
		self terminate.
		 ] ifFalse: [  
	(aContext isDead) ifTrue:
		[^1 to: 0].
	^aContext debuggerMap
		rangeForPC: aContext pc
		contextIsActiveContext: (self isLatestContext: aContext) ]