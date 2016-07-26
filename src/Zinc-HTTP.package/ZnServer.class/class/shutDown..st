system startup
shutDown: quiting
	"Our system shutDown hook: stop all servers we manage"
	
	(quiting or: [ self alwaysRestart ])
		ifTrue: [
			self managedServers do: [ :each | each stop: false ] ]