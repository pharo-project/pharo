accessing
adoptPaneColor: paneColor
	"Pass on to the content morph a little lighter."
	
	paneColor ifNil: [^super adoptPaneColor: paneColor].
	super adoptPaneColor: (self theme subgroupColorFrom: paneColor).
	self contentMorph borderStyle: (self theme tabPanelBorderStyleFor: self)