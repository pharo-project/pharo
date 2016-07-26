private-announcers
stopMonticelloAnnouncements
	self announcer 
		unsubscribe: MCVersionLoaderStarted thoroughlyfor: self;
		unsubscribe: MCVersionLoaderStopped thoroughlyfor: self.
	monticelloAnnouncements := false