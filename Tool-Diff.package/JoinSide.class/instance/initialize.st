initialization
initialize
	"Initialize the receiver."

	super initialize.
	self
		highlights: #();
		offset: 0@0;
		range: (1 to: 1);
		lineRange: (1 to: 0);
		color: Color yellow;
		text: ''