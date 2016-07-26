converting
makeStrong

	| sub |
	sub := self subscriber.
	sub ifNil: [ ^ self error: 'Subscriber is nil, cannot make strong subscription' ].
	
	^ announcer replace: self with: 
		(AnnouncementSubscription new
			announcer: announcer;
			action: action asMessageSend;
			subscriber: sub;
			announcementClass: announcementClass)
