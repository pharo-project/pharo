private
setStopConditions
	"Set the font and the stop conditions for the current run."
	
	self setFont.
	stopConditions := alignment = Justified
		ifTrue: [PaddedSpaceCondition]
		ifFalse: [DefaultStopConditions]