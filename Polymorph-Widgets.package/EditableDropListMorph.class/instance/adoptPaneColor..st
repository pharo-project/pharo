accessing
adoptPaneColor: paneColor
	"Clear the fill style of the text."
	
	super adoptPaneColor: paneColor.
	self contentMorph fillStyle: Color transparent 