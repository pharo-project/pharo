context
activePC: aContext 
	^ (self isLatestContext: aContext)
		ifTrue: [ interruptedContext pc ]
		ifFalse: [ self previousPC: aContext ].