methods
createBreakCommand
	<globalCommand>
	^ SugsBreakAlwaysSuggestion
		icon: (Smalltalk ui icons iconNamed: #smallCancelIcon)
		position: 15
		keyText: 'b,a'