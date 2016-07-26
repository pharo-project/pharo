system startup
shutDown: aboutToQuit
	self allInstancesDo: #cleanUp