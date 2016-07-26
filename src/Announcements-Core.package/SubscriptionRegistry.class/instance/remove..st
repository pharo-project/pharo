add/remove
remove: subscription
	^ self protected: [
		subscriptions remove: subscription ifAbsent: nil ]

