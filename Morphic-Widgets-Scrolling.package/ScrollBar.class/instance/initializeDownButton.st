initialization
initializeDownButton
	"initialize the receiver's downButton"

	downButton := self theme newScrollBarDownButtonFor: self.
	
	downButton color: self thumbColor.
	downButton 
		on: #mouseDown
		send: #scrollDownInit
		to: self.
	downButton 
		on: #mouseUp
		send: #finishedScrolling
		to: self.
	self updateDownButtonImage.
	downButton 
		borderWidth: 1;
		borderColor: Color lightGray.
		
	downButton cornerStyle: (self theme scrollbarButtonCornerStyleIn: self window).
	downButton on: #mouseUp send: #finishedScrolling: to: self.
	downButton on: #mouseEnter send: #mouseEnterDownButton: to: self.
	downButton on: #mouseLeave send: #mouseLeaveDownButton: to: self.
	self addMorph: downButton