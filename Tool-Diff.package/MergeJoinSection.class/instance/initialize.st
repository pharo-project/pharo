initialization
initialize
	"Initialize the receiver."

	self
		allowClick: true;
		selected: false;
		selectionState: #dst;
		stateIcons: self defaultStateIcons;
		selectedBorderColor: Color black.
	super initialize