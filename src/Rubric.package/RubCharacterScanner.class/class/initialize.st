class initialization
initialize
"
	RubCharacterScanner initialize
"
	| a |
	a := RubTextStopConditions new.
	a at: 1 + 1 put: #embeddedObject.
	a at: Character tab asciiValue + 1 put: #tab.
	a at: Character cr asciiValue + 1 put: #cr.
	a at: Character lf asciiValue + 1 put: #cr.
	
	NilCondition := a copy.
	DefaultStopConditions := a copy.

	PaddedSpaceCondition := a copy.
	PaddedSpaceCondition at: Character space asciiValue + 1 put: #paddedSpace.
	
	SpaceCondition := a copy.
	SpaceCondition at: Character space asciiValue + 1 put: #space.
