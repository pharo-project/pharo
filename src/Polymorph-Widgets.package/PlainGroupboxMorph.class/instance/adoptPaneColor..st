accessing
adoptPaneColor: paneColor
	"Change our color too."
	
	super adoptPaneColor: (self theme subgroupColorFrom: paneColor).
	self borderStyle: (self theme plainGroupPanelBorderStyleFor: self)