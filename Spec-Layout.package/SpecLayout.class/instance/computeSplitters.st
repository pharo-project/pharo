private
computeSplitters

	(self commands select: [ :e | e isSplitter ])
		do: [ :e | 
			shouldCheckSplitters := true.
			e generateArguments.
			e asSpecElements do: [ :el | result add: el ] ].
	shouldCheckSplitters
		ifTrue: [ result add: #checkSplitters ]