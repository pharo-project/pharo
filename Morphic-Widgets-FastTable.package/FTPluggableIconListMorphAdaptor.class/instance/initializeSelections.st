initialization
initializeSelections
	self 
		onAnnouncement: FTSelectionChanged  
		send: #selectionChanged: 
		to: self.
	self 
		onAnnouncement: FTStrongSelectionChanged  
		send: #strongSelectionChanged: 
		to: self.	