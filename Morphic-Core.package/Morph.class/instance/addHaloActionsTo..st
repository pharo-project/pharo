menus
addHaloActionsTo: aMenu
	"Add items to aMenu representing actions requestable via halo"

	| subMenu |
	subMenu := UIManager default newMenuIn: self for: self.
	subMenu 
	buildTitle: [ :menuTitle | menuTitle 
		bigTitle: self externalName
	];
	add: 'delete' selector: #dismissViaHalo.
	subMenu balloonTextForLastItem: 'Delete this object -- warning -- can be destructive!' translated.

	self maybeAddCollapseItemTo: subMenu.
	subMenu add: 'grab' selector: #openInHand.
	subMenu balloonTextForLastItem: 'Pick this object up -- warning, since this removes it from its container, it can have adverse effects.' translated.

	subMenu addLine.

	subMenu add: 'resize'  selector: #resizeFromMenu.
	subMenu balloonTextForLastItem: 'Change the size of this object' translated.

	subMenu add: 'duplicate'  selector: #maybeDuplicateMorph.
	subMenu balloonTextForLastItem: 'Hand me a copy of this object'.

	subMenu addLine.

	subMenu add: 'set color'  target: self renderedMorph selector: #changeColor.
	subMenu balloonTextForLastItem: 'Change the color of this object'.

	subMenu addLine.

	subMenu add: 'inspect' target: self selector: #inspect.
	subMenu balloonTextForLastItem: 'Open an Inspector on this object'.

	aMenu add: 'halo actions...' subMenu: subMenu
