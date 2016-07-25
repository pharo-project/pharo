initialization
initialize
	super initialize.
	
	focus := Dictionary new.
	currentScrollDelta := 0.
	maxPanes := 2.
	self band: self newBand.
	Morph announcer weak 
		when: MorphGotFocus send: #focusChanged: to: self.