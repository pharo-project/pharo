ui-building
newMenuWith: menuItems
	"Answer menu with the given morphs."

	| menu |
	menu := self newMenu.
	menu cornerStyle: #square.
	menuItems do: [:m | menu addMorphBack: m].
	menu
		borderWidth: 0;
		color: self baseColor;
		borderColor:  Color transparent;
		stayUp: true;
		beSticky;
		removeDropShadow;
		popUpOwner: (MenuItemMorph new privateOwner: self).
	^ menu