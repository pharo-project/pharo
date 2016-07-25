private
setStopConditions
	"Set the font and the stop conditions for the current run."
	
	self setFont.
	stopConditions := wantsColumnBreaks == true
		ifTrue: [ColumnBreakStopConditions]
		ifFalse: [CompositionStopConditions]