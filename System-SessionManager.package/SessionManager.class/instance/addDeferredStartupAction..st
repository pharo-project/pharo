deferred startup actions
addDeferredStartupAction: aBlock
	"Add the block to the list of actions that we will be performed immediately after the startup list is executed."
	self currentSession 
		addDeferredStartupAction: aBlock.
	