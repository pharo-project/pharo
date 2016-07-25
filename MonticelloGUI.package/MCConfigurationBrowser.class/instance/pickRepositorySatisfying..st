morphic ui
pickRepositorySatisfying: aBlock
	| index list |
	list := MCRepositoryGroup default repositories select: aBlock.
	index := (UIManager default chooseFrom: (list collect: [:ea | ea description]) title: 'Repository:' translated).
	^ index = 0 ifFalse: [list at: index]