testing
hasItems
	"Answer if the receiver has menu items"
	^ submorphs anySatisfy: [:each | each isMenuItemMorph ]
