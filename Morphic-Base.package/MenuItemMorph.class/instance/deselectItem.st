meta actions
deselectItem
	| item |
	self isSelected: false.
	subMenu ifNotNil: [subMenu deleteIfPopUp].
	(owner isMenuMorph) ifTrue:
		[item := owner popUpOwner.
		(item isMenuItemMorph) ifTrue: [item deselectItem]].
