private
send: treeMorph mouseDownWithShift: aBoolean onItem: treeNodeMorph
	| mouseUp |
	mouseUp := MouseButtonEvent basicNew 
		setType: #mouseDown
		position: (treeNodeMorph bounds origin + (1@1))
		which: 4
		buttons: (aBoolean ifTrue: [8] ifFalse: [0])
		hand: (HandMorph new mouseFocus: treeNodeMorph; yourself)
		stamp: nil.
	treeMorph mouseUp: mouseUp onMorph: treeNodeMorph.