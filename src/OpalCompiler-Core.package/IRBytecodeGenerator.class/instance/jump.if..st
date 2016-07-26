instructions
jump: distance if: condition
	distance = 0
		ifTrue: [ ^ encoder genPop ].
	condition
		ifTrue: [ encoder genBranchPopTrue: distance ]
		ifFalse: [ encoder genBranchPopFalse: distance ]
		