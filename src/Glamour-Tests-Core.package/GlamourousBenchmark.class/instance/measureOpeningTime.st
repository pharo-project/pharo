utilities
measureOpeningTime
	| browser numberOfOpenings time |
	numberOfOpenings := 15.

	browser := self defaultBrowser.
	time := Time millisecondsToRun: [ numberOfOpenings timesRepeat: [(browser openOn: 10) delete] ].
	^ {numberOfOpenings . time}