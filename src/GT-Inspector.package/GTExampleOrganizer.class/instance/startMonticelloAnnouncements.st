private-announcers
startMonticelloAnnouncements
	monticelloAnnouncements == true ifTrue: [ ^ self ]. " handle: nil | false | true / avoid multiple subscriptions "
	self announcer
		when: MCVersionLoaderStarted send: #loadStarted: to: self;
		when: MCVersionLoaderStopped send: #loadStopped: to: self.
	monticelloAnnouncements := true