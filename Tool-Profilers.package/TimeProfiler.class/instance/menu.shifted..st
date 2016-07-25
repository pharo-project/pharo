UI specific
menu: menu shifted: b 
	"Set up the menu to apply to the receiver's, honoring the #shifted boolean"
	
	self selectedNode 
		ifNotNil: [ :current | 
			current menu: menu shifted: b.
			menu addLine.
			menu
				add: 'Expand all from here'
				target: self
				selector: #expandAllFromNode:
				argument: current ].
	^ menu