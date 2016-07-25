UI specific
showLeavesOnly: aBoolean
	self showLeavesOnly ~= aBoolean
		ifFalse: [ ^ self ].
	treeMorph
		treeLineWidth:
			(aBoolean
				ifTrue: [ 0 ]
				ifFalse: [ 1 ]).
	showLeavesOnly := aBoolean.
	self changed: #showLeavesOnly.
	self updateList