*Morphic-Base
startThenBrowseMessageTally
	"Tally only the UI process"
	
	(self confirm: 'MessageTally the UI process until the
mouse pointer goes to the top of the screen')
		ifTrue: [TimeProfiler
				onBlock: [[World activeHand position y > 10]
						whileTrue: [World doOneCycle]]]