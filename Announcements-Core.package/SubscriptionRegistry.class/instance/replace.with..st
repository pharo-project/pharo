add/remove
replace: subscription with: newOne

	" Note that it will signal an error if subscription is not there "
	self protected: [
		subscriptions remove: subscription.
		subscriptions add: newOne 
	].
	^ newOne