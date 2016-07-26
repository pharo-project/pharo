announcing
deliver: anAnnouncement
	|  interestedSubscriptions |
	
	"using a copy, so subscribers can unsubscribe from announcer "
	
	subscriptions isEmpty ifTrue: [ ^ self ].
	self protected: [ 
		interestedSubscriptions := self subscriptionsHandling: anAnnouncement ].
	self deliver: anAnnouncement to: interestedSubscriptions