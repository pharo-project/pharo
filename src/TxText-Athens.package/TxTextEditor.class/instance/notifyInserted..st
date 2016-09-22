private notifying
notifyInserted: aString
	self announce: (TxInsertedTextAnnouncement new
		insertedText: aString;
		editor: self;
		yourself)