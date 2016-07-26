initialization
registerInterestToSystemAnnouncement
	<systemEventRegistration>
	uniqueInstance ifNil: [ ^ self ].
	SystemAnnouncer uniqueInstance
		unsubscribe: uniqueInstance.
	SystemAnnouncer uniqueInstance weak subscribe: PrivacyChangedAnnoucement
			send: #privacyChanged 
			to: uniqueInstance.