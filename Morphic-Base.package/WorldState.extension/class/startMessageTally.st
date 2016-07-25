*Morphic-Base
startMessageTally
	(self confirm: 'MessageTally will start now,
and stop when the cursor goes
to the top of the screen' translated) 
		ifTrue: [TimeProfiler spyAllOn: [[World activeHand position y > 0] 
					whileTrue: [World doOneCycle]]]