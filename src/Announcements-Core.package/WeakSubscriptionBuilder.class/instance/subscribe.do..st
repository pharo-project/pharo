private
subscribe: anAnnouncementClass do: aValuable 
	"Do not use this message on weak announcer because it does not work. The block will hold strongly the receiver and more.
	 We need ephemerons for that'"
	"aValuable isBlock ifTrue: [  
		self error: 'Do not use this message on weak and block because it does not work. We need ephemerons for that']."
	^	announcer basicSubscribe: (
			LegacyWeakSubscription new 
				announcer: announcer;
				announcementClass: anAnnouncementClass;
				valuable: aValuable)