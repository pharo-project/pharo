menu
addNavigationMenuItemsTo: aMenu

	criticsModel selectedItem ifNotNil: [ :critique |
		critique sourceAnchor entity populateCBMenu: aMenu ]