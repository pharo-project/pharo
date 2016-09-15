methods
createBreakCommand
	<globalCommand>
	^ SugsBreakAlwaysSuggestion
		icon: (self iconNamed: #smallCancelIcon)
		position: 15
		keyText: 'b,a'