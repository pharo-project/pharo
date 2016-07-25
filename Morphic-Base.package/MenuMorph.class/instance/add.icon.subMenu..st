construction
add: aLabelString icon: aForm subMenu: aMenuMorph
	"Append the given submenu with the given label."

	self addToggle: aLabelString target: nil selector: nil.
	self lastItem
		icon: aForm;
		subMenu: aMenuMorph.
	^ self