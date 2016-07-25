construction
add: wordingString icon: aForm help: helpString subMenu: aMenuMorph 
	"Append the given submenu with the given label."
	| item |
	item := DockingBarMenuItemMorph new.
	item contents: wordingString.
	item subMenu: aMenuMorph.
	item icon: aForm.
	helpString ifNotNil: [item setBalloonText: helpString].
	self addMorphBack: item