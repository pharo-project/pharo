converting
makeWeak
	
	action isBlock ifTrue: [
		self error: 'Not currently available due to missing ephemerons support'
		].

	^ announcer replace: self with: 
		(LegacyWeakSubscription new
			announcer: announcer;
			action: action asWeakMessageSend;
			subscriber: subscriber;
			announcementClass: announcementClass)