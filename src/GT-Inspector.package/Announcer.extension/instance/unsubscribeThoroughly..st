*GT-Inspector-Examples-Core
unsubscribeThoroughly: aSubscriber
	^ registry protected: [
		registry subscriptions removeAllSuchThat: [ :subscription | 
			subscription subscriber class == aSubscriber class " class !!! " ] ]
