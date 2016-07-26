announcements
setSpecificAnnouncementActionsTo: aRenderer

	self specificAnnouncementActions keysAndValuesDo: [:anAnnouncementClass :aSymbol |
		self when: anAnnouncementClass send: aSymbol to: aRenderer.
		]