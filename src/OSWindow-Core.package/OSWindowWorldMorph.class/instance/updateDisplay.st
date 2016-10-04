as yet unclassified
updateDisplay
	display isNil ifTrue: [
		display := Form extent: self extent depth: 32.
		worldState display: display
	]