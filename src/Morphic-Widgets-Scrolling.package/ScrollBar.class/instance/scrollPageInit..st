access
scrollPageInit: evt
	"Scroll initiated by the paging area."
	
	self resetTimer.
	self setNextDirectionFromEvent: evt.
	self scrollBarAction: #doScrollByPage.
	self doScrollByPage. "do the first one now since morph stepping is rather variable in its timing
		and the user may release before actually actioned...."
	pagingArea
		fillStyle: self pressedFillStyle;
		borderStyle: self pressedBorderStyle.
	self startStepping: #stepAt: at: Time millisecondClockValue + self stepTime arguments: nil stepTime: nil