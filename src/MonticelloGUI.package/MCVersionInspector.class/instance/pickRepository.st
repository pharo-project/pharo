morphic ui
pickRepository
	| index |
	index := UIManager default chooseFrom: (self repositories collect: [:ea | ea description])
				title: 'Repository:'.
	^ index = 0 ifFalse: [self repositories at: index]