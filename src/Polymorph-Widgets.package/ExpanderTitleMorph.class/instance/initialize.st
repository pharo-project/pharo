initialization
initialize
	"Initialize the receiver."

	super initialize.
	self
		expanded: false;
		changeTableLayout;
		borderStyle: self defaultBorderStyle;
		layoutInset: (self theme expanderTitleInsetFor: self);
		listDirection: #leftToRight;
		listCentering: #center;
		wrapCentering: #center;
		buttonMorph: self newExpandButtonMorph;
		addMorph: self buttonMorph;
		labelMorph: self newLabelMorph;
		on: #mouseUp send: #toggleExpanded to: self