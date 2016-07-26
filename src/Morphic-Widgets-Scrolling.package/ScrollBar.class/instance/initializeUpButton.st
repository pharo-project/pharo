initialization
initializeUpButton
	"initialize the receiver's upButton"
	upButton := self theme newScrollBarUpButtonFor: self.
	upButton color: self thumbColor.
	upButton
		on: #mouseDown
		send: #scrollUpInit
		to: self.
	upButton
		on: #mouseUp
		send: #finishedScrolling
		to: self.
	self updateUpButtonImage.
	upButton 
		borderWidth: 1;
		borderColor: Color lightGray.
		
	upButton cornerStyle: (self theme scrollbarButtonCornerStyleIn: self window).
	upButton on: #mouseUp send: #finishedScrolling: to: self.
	upButton on: #mouseEnter send: #mouseEnterUpButton: to: self.
	upButton on: #mouseLeave send: #mouseLeaveUpButton: to: self.
	self addMorph: upButton