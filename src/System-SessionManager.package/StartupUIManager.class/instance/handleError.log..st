error handling
handleError: anError log: shouldLog
	"We should log only the first time that the error is handled. Then, the deferred startup action never asks to log: either we never were asked to log (shouldLog=true) or we logged in the super send."

	self flag: 'TODO'. "Maybe we should only catch Errors and not Exceptions in WorkingSession>>#runList:do:"
	
	super handleError: anError log: shouldLog.
	(anError isKindOf: Notification) 
		ifTrue:  [ anError resume ] 
		ifFalse: [ anError freeze.
						self sessionManager
						addDeferredStartupAction: [ self uiManager handleError: anError log: false ]].