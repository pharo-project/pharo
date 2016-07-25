drawing
listPaneColor: paneColor
	"Set the pane color for the list."
	
	self listMorph ifNil: [^self].
	self listMorph
		adoptPaneColor: paneColor;
		fillStyle: (self theme dropListNormalListFillStyleFor: self);
		borderStyle: (self theme dropListNormalListBorderStyleFor: self)