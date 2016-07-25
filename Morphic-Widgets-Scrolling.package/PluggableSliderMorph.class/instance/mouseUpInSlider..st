other events
mouseUpInSlider: event 

	super mouseUpInSlider: event.
	"label or label position may change"
	self updateLabel.