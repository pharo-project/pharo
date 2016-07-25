source text events
whenTextAreaSelectionChanged: aSelectionChangedAnnounce
	self selectionInterval: aSelectionChangedAnnounce morph textArea selectionInterval.
	contentSelection :=  aSelectionChangedAnnounce morph textArea selection