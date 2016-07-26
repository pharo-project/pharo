scrolling
scrollAbsolute: event
	"Ignore if disabled."
	
	self enabled ifFalse: [^self].
	super scrollAbsolute: event.
	"label or label position may change"
	self updateLabel.