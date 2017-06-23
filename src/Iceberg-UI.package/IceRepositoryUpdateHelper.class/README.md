I am a helper to create update cycles on repositories browser.
My purpose is to allow weaks to work properly, since the browser needs to do something like this: 
	
	Iceberg announcer weak 
		subscribe: IceRepositoryCreated 
		do: [ :ann | 
			table 
				updateOn: IceRepositoryAnnouncement 
				from: ann repository announcer ].

... but the problem is that blocks and weak announcements do not work well, because when declaring a block you take all the outer context with you, hence creating a de-facto strong reference . 
So we need a replacement, and I'm that replacement :)
Then, that code will look like this: 

	Iceberg announcer weak 
		subscribe: IceRepositoryCreated 
		send: #execute:
		to: (IceRepositoryUpdateHelper for: table).

removing the outer context problems :)