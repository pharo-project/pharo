asserting
assertDebuggerInitializationForContext: aContext andStepBlock: aBlock

	self 
		initializeSessionForContext: aContext
		withStepBlock: aBlock.

	self assertDebuggerInitialization.