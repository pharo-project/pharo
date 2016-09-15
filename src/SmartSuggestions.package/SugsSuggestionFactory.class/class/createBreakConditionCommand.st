methods
createBreakConditionCommand
	<globalCommand>
	^ SugsBreakConditionSuggestion
		icon: (self iconNamed: #smallCancelIcon)
		position: 17
		keyText: 'b,c'