initialization
initializeAnnouncer
	announcer 
		ifNil: [ announcer := Announcer new ]
		ifNotNil: [	announcer subscriptions reset ].
	announcer weak
		when: IceCommited, IcePackageLoaded 
		send: #refresh 
		to: self