private
send: treeMorph mouseUpWithShift: aBoolean onItem: treeNodeMorph
	| mouseUp |
	mouseUp := MouseButtonEvent basicNew 
		setType: #mouseUp
		position: (treeNodeMorph bounds origin + (1@1))
		which: 4
		buttons: (aBoolean ifTrue: [8] ifFalse: [0])
		hand: (HandMorph new mouseFocus: treeNodeMorph; yourself)
		stamp: nil.
	treeMorph listManager mouseUp: mouseUp on: treeNodeMorph.