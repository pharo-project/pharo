initialization
initialize
	"Initialize the receiver."

	super initialize.
	self
		src: JoinSide new;
		dst: JoinSide new;
		shape: Polygon new;
		width: 0;
		borderWidth: 0;
		borderColor: Color transparent;
		type: #modification