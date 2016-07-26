morphic ui
pickWorkingCopySatisfying: aBlock
	| copies index |
	copies := self workingCopies select: aBlock.
	copies isEmpty ifTrue: [ ^nil ].
	index := UIManager default chooseFrom: (copies collect: [:ea | ea packageName])
				title: 'Package:'.
	^ index = 0 ifFalse: [ copies at: index]