initialization
initializePagingArea
	"Initialize the receiver's pagingArea."
	
	pagingArea := Morph
				newBounds: self totalSliderArea
				color: (Color
						r: 0.6
						g: 0.6
						b: 0.8).
	pagingArea
		on: #mouseDown
		send: #scrollPageInit:
		to: self.
	pagingArea
		on: #mouseUp
		send: #finishedScrolling
		to: self.
	self addMorph: pagingArea.
	pagingArea cornerStyle: (self theme scrollbarPagingAreaCornerStyleIn: self window).
	pagingArea on: #mouseUp send: #finishedScrolling: to: self.
	self on: #mouseEnter send: #mouseEnterPagingArea: to: self.
	self on: #mouseLeave send: #mouseLeavePagingArea: to: self