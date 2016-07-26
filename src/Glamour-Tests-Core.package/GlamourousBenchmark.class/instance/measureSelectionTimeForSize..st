utilities
measureSelectionTimeForSize: size
	"
	self new measureSelectionTime 
	"
	| browser numberOfSelections time port window |
	numberOfSelections := 15.

	browser := self defaultBrowser.
	window := browser openOn: size.
	port := browser panes first ports second.
	time := Time millisecondsToRun: [ (size - numberOfSelections) to: size do: [:i | port value: i ] ].
	window delete.
	^ {size . time}