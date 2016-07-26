initialization
initialize
	"Initialize the receiver."

	super initialize.
	
	labelClickable := true.
	
	self
		borderWidth: 2; "space for focus"
		borderColor: Color transparent;
		enabled: true;
		changeTableLayout;
		listDirection: #leftToRight;
		wrapCentering: #center;
		cellInset: 4;
		labelMorph: self newLabelMorph;
		buttonMorph: self newButtonMorph;
		on: #click send: #updateButton: to: self;
		on: #mouseMove send: #updateButton: to: self;
		on: #mouseUp send: #updateButton: to: self