system startup
shutDown: quitting
	"Force my instances to flush before image quits"

	quitting ifTrue: [ 
		self allInstancesDo: [:each | each flush ] ].