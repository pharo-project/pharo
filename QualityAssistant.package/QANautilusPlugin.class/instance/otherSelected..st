announcement handling
otherSelected: anAnnouncement

	 (anAnnouncement class == NautilusPackageSelected or:
	[ anAnnouncement class == NautilusProtocolSelected ]) ifFalse: [ ^ self ].

	currentEntity := nil.
	self updateMorph  