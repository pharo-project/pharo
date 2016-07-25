stop conditions
setStopConditions
	"Set the font and the stop conditions for the current run."
	
	self setFont.
	self setConditionArray: (alignment = self justified ifTrue: [#paddedSpace]).

"
	alignment = self justified ifTrue: [
		stopConditions == DefaultStopConditions 
			ifTrue:[stopConditions := stopConditions copy].
		stopConditions at: Character space asciiValue + 1 put: #paddedSpace]
"