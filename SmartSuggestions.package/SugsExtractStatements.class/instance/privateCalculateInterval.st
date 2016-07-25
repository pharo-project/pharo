private
privateCalculateInterval
	| selectedValidCodeInterval selectedValidCodeStatements |
	selectedValidCodeStatements := context selectedStatementsAsSequence.
	selectedValidCodeInterval := selectedValidCodeStatements start to: selectedValidCodeStatements stop.
	^ selectedValidCodeInterval