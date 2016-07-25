context
filterTopContext: aContext
	|ctx|
	 
	ctx := aContext.
	(self shouldDisplayOnTopContext: ctx) ifTrue: [^ ctx].
	[(ctx := ctx sender) ~~ nil] whileTrue:
		[(self shouldDisplayOnTopContext: ctx) ifTrue: [^ ctx]].
	^ nil