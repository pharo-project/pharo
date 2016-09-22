private notifying
notifyNewText
	self announce: (TxNewTextAnnouncement new
		editor: self;
		yourself)