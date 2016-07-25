layout-menu
addLayoutMenuItems: topMenu hand: aHand
	| aMenu |
	aMenu := UIManager default newMenuIn: self for: self.
	aMenu addUpdating: #hasNoLayoutString selector: #changeNoLayout.
	aMenu addUpdating: #hasProportionalLayoutString selector: #changeProportionalLayout.
	aMenu addUpdating: #hasTableLayoutString selector: #changeTableLayout.
	aMenu addLine.
	aMenu add: 'change layout inset...' selector: #changeLayoutInset:.
	aMenu addLine.
	self addCellLayoutMenuItems: aMenu hand: aHand.
	self addTableLayoutMenuItems: aMenu hand: aHand.
	topMenu ifNotNil: [ topMenu add: 'layout' subMenu: aMenu ].
	^aMenu