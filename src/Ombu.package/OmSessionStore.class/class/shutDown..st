system startup
shutDown: quitting
	"Force my instances to flush to disk before image quits"

	quitting ifTrue: [ 
		self allInstancesDo: [:each | each store flush ] ].