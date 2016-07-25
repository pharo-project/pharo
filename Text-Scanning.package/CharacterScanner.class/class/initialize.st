class initialization
initialize
"
	CharacterScanner initialize
"
	| a |
	a := Array new: 258.
	a at: 1 + 1 put: #embeddedObject.
	a at: Tab asciiValue + 1 put: #tab.
	a at: CR asciiValue + 1 put: #cr.
	a at: Character lf asciiValue + 1 put: #cr.
	"Note: following two codes are used only by primitive 103 for accelerated Character scanning"
	a at: 257 put: #endOfRun.
	a at: 258 put: #crossedX.
	
	DefaultStopConditions := a copy.

	CompositionStopConditions := a copy.
	CompositionStopConditions at: Space asciiValue + 1 put: #space.
	ColumnBreakStopConditions := CompositionStopConditions copy.
	ColumnBreakStopConditions at: TextComposer characterForColumnBreak asciiValue + 1 put: #columnBreak.

	PaddedSpaceCondition := a copy.
	PaddedSpaceCondition at: Space asciiValue + 1 put: #paddedSpace.

	MeasuringStopConditions := (Array new: 258)
		at: 257 put: #endOfRun;
		at: 258 put: #crossedX;
		yourself