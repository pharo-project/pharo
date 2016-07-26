system startup
startUp: resuming
	"Our system startUp hook: start all servers we manage.
	We do this using deferred startup actions to allow normal error handling."

	(resuming or: [ self alwaysRestart ])
		ifTrue: [
			self environment 
				at: #SessionManager 
				ifPresent: [ :manager | 
					manager default currentSession addDeferredStartupAction: [ 
						self managedServers do: [ :each | each start ] ] ]
				ifAbsent: [ 
					Smalltalk addDeferredStartupAction: [ 
						self managedServers do: [ :each | each start ] ] ] ]