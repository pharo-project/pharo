tests
testTextBackgroundColor
	| browser firstTreeMorph secondTreeMorph treeNodeMorph txtMorph |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit
		to: #one;
		andShow: [ :a | 
					a list
						display: [ :x | 1 to: x ];
						textBackgroundColor: [ :aNumber | 
									(aNumber isDivisibleBy: 2)
										ifTrue: [ Color green ]
										ifFalse: [ Color red ] ] ].
	window := browser openOn: 4.
	firstTreeMorph := window submorphs last submorphs first submorphs first submorphs first.
	treeNodeMorph := firstTreeMorph scroller submorphs first.
	txtMorph := treeNodeMorph submorphs first submorphs first submorphs first.
	self assert: (txtMorph isKindOf: TextMorph).
	self assert: txtMorph backgroundColor equals: Color red.
	treeNodeMorph := firstTreeMorph scroller submorphs second.
	txtMorph := treeNodeMorph submorphs first submorphs first submorphs first.
	self assert: (txtMorph isKindOf: TextMorph).
	self assert: txtMorph backgroundColor equals: Color green.
	treeNodeMorph := firstTreeMorph scroller submorphs third.
	txtMorph := treeNodeMorph submorphs first submorphs first submorphs first.
	self assert: (txtMorph isKindOf: TextMorph).
	self assert: txtMorph backgroundColor equals: Color red.
	treeNodeMorph := firstTreeMorph scroller submorphs fourth.
	txtMorph := treeNodeMorph submorphs first submorphs first submorphs first.
	self assert: (txtMorph isKindOf: TextMorph).
	self assert: txtMorph backgroundColor equals: Color green