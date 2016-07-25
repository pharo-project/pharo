morphic ui
pickAncestor
	| index versions |
	versions := self version info breadthFirstAncestors.
	index := UIManager default chooseFrom: (versions collect: [:ea | ea name])
				title: 'Ancestor:'.
	^ index = 0 ifFalse: [versions at: index]