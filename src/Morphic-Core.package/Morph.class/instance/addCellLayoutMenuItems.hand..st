layout-menu
addCellLayoutMenuItems: aMenu hand: aHand
	"Cell (e.g., child) related items"
	| menu sub |
	menu := UIManager default newMenuIn: self for: self.
	menu addUpdating: #hasDisableTableLayoutString selector: #changeDisableTableLayout.
	menu addLine.
	sub := UIManager default newMenuIn: self for: self.
	#(#rigid #shrinkWrap #spaceFill)
		do: [ :sym | 
			sub
				addUpdating: #hResizingString:
				target: self
				selector: #hResizing:
				argumentList: (Array with: sym) ].
	menu add: 'horizontal resizing' subMenu: sub.
	sub := UIManager default newMenuIn: self for: self.
	#(#rigid #shrinkWrap #spaceFill)
		do: [ :sym | 
			sub
				addUpdating: #vResizingString:
				target: self
				selector: #vResizing:
				argumentList: (Array with: sym) ].
	menu add: 'vertical resizing' subMenu: sub.
	aMenu ifNotNil: [ aMenu add: 'child layout' subMenu: menu ].
	^ menu