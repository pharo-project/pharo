stop conditions
setStopConditions
	"Set the font and the stop conditions for the current run."
	
	self setFont.
	self setConditionArray: (alignment = Justified ifTrue: [#paddedSpace]).
