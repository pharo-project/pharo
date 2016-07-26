*GT-Inspector-Examples-Core
unsubscribe: anAnnouncementClass thoroughlyfor: aSubscriber
	^ registry protected: [
		registry subscriptions removeAllSuchThat: [ :subscription | 
			subscription subscriber class == aSubscriber class " class !!! "
			 and: [ subscription announcementClass == anAnnouncementClass ] ] ]
