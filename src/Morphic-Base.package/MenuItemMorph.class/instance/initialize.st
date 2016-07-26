initialization
initialize
	"initialize the state of the receiver"
	super initialize.
	""
	
	contents := ''.
	hasFocus := false.
	isEnabled := true.
	subMenu := nil.
	isSelected := false.
	target := nil.
	selector := nil.
	arguments := nil.
	font := StandardFonts menuFont.
	self hResizing: #spaceFill;
		 vResizing: #shrinkWrap