event handling
mouseUp: evt 
	"Handle a up event. Show the location morph again."
	
	evt hand showTemporaryCursor: nil.
	self updateSelectedLocation.
	self locationMorph visible: true