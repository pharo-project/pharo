temps
fixTemp: name
	| node |
	node := scopeTable at: name ifAbsent: [].
	node class ~~ TempVariableNode ifTrue:
		[self error: 'can only fix a floating temp var'].
	node index: nTemps.
	nTemps := nTemps + 1.
	^node