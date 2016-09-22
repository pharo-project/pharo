private notifying
notifySelectionChange: aSelection
	self announce: (TxSelectionChangedAnnouncement new
		editor: self;
		newSelection: aSelection;
		yourself)