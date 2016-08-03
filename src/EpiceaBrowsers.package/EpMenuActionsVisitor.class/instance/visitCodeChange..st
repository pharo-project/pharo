visitor
visitCodeChange: aChange
	"Answer common operations for code changes"
		
	^ (self visitEvent: aChange)
		add: self browseMenuAction;
		yourself