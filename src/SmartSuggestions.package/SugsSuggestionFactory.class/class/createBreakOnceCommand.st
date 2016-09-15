methods
createBreakOnceCommand
	<globalCommand>
	^ SugsBreakOnceSuggestion
		icon: (self iconNamed: #smallCancelIcon)
		position: 16
		keyText: 'b,o'