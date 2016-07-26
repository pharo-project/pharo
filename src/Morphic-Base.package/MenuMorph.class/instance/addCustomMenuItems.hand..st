menu
addCustomMenuItems: aCustomMenu hand: aHandMorph

	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu addLine.
	aCustomMenu add: 'add title...' selector: #addTitle.
	aCustomMenu add: 'set target...' selector: #setTarget:.
	defaultTarget ifNotNil: [
		aCustomMenu add: 'add item...' selector: #addItem].
	aCustomMenu add: 'add line' selector: #addLine.
	(self items anySatisfy: [ :any | any hasSubMenu ]) ifTrue: [
		 aCustomMenu add: 'detach submenu' selector: #detachSubMenu: ].