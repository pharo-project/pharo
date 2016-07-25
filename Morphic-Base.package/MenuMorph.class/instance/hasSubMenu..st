testing
hasSubMenu: aMenuMorph

	^submorphs anySatisfy: [ :m | m isMenuItemMorph and: (m hasSubMenu: aMenuMorph) ]
