launching
askForEmergencyEvaluatorOrExitWithText: aText 
	| m |
	m := String streamContents: [:s | 
					s nextPutAll: aText.
					s cr; nextPutAll: '-----------------------------------------------------------------'.
					s cr; nextPutAll: 'Type CR to enter an emergency evaluator.'.
					s cr; nextPutAll: 'Type any other character to restart.'].
	Display getCanvas 
		drawString: m
		from: 1
		to: m size
		autoBoundAt: 0 @ 0
		font: nil
		color: Color black.
	self nextChar = Character cr
		ifTrue: [self emergencyEvaluator]