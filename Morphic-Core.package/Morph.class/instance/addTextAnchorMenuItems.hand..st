text-anchor
addTextAnchorMenuItems: topMenu hand: aHand
	| aMenu |
	aMenu := UIManager default newMenuIn: self for: self.
	aMenu addUpdating: #hasInlineAnchorString selector: #changeInlineAnchor.
	aMenu addUpdating: #hasParagraphAnchorString selector: #changeParagraphAnchor.
	aMenu addUpdating: #hasDocumentAnchorString selector: #changeDocumentAnchor.
	topMenu ifNotNil: [ topMenu add: 'text anchor' subMenu: aMenu ].
	^aMenu