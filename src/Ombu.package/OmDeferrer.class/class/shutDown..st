system startup
shutDown: quitting
	"Force my instances to flush before image quits"

	self allInstancesDo: [:each | each flush ].