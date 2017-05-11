system startup
shutDown: aboutToQuit

	aboutToQuit ifTrue: [ self reset ]