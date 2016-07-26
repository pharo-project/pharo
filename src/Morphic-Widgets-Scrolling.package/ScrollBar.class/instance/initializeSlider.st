initialization
initializeSlider
	"Initialize the receiver's slider."
	
	self initializeUpButton; initializeDownButton; initializePagingArea.
	super initializeSlider.
	self sliderColor: self sliderColor.
	self slider
		width: self theme scrollbarThickness;
		height: self theme scrollbarThickness.
	slider cornerStyle: (self theme scrollbarThumbCornerStyleIn: self window).
	slider on: #mouseEnter send: #mouseEnterThumb: to: self.
	slider on: #mouseLeave send: #mouseLeaveThumb: to: self