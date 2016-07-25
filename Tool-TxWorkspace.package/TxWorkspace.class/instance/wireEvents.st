opening
wireEvents
	| textEditor |
	
	textEditor := editor controller.
	
	textEditor announcer when: TxDeletedTextAnnouncement send: #onTextDeleted: to: self.
	textEditor announcer when: TxInsertedTextAnnouncement send: #onTextInserted: to: self.