accessing
adoptPaneColor: paneColor
	"Pass on to the content morph."
	
	|c|
	paneColor ifNil: [^super adoptPaneColor: paneColor].
	c := self theme subgroupColorFrom: paneColor.
	super adoptPaneColor: c.
	self contentMorph borderStyle: (self theme groupPanelBorderStyleFor: self).
	self labelMorph
		color: paneColor blacker muchDarker;
		backgroundColor: c;
		borderStyle: (self theme groupLabelBorderStyleFor: self)