add/remove
removeSubscriber: subscriber
	
	^ self protected: [
		subscriptions removeAllSuchThat: [:subscription | subscription subscriber == subscriber ]]

