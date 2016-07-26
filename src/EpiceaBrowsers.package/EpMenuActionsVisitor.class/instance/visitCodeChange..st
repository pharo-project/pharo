visitor
visitCodeChange: aChange
	"Answer common operations for code changes"
		
	^ (self visitEvent: aChange)
		add: self undoMenuAction;
		add: self redoMenuAction;
		add: self browseMenuAction;
		yourself