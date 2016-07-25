accessing
menuItems
	^submorphs select: [ :m | m isMenuItemMorph ]
