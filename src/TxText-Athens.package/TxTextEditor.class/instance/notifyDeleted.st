private notifying
notifyDeleted
	self announce: (TxDeletedTextAnnouncement new
		editor: self;
		yourself)