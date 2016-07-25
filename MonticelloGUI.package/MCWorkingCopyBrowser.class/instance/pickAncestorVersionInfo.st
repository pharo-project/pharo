morphic ui
pickAncestorVersionInfo
	| ancestors index |
	ancestors := workingCopy ancestry breadthFirstAncestors.
	index := UIManager default chooseFrom: (ancestors collect: [:ea | ea name])
				title: 'Ancestor:'.
	^ index = 0 ifFalse: [ ancestors at: index]