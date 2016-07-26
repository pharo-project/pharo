debugging actions
restart: aContext
	"Proceed from the initial state of selectedContext." 
	"Closing now depends on a setting (RestartAlsoProceeds class variable) --> not supported in this version"

	(self isContextPostMortem: aContext) ifTrue: [^ self].
	self unwindAndRestartToContext: aContext.
	
	"Issue 3015 - Hernan"
	self isInterruptedContextATest ifTrue: [ self prepareTestToRunAgain ].
	
	self triggerEvent: #restart		
	