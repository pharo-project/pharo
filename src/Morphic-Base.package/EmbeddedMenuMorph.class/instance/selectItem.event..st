control
selectItem: aMenuItem event: anEvent
	"Deselect any sibling menus."
	
	|menus|
	menus := self owner submorphs select: [:m | (m isKindOf: self class) and: [m ~~ self]].
	menus do: [:menu |
		menu perform: #selectItem:event: withArguments: {nil. anEvent} inSuperclass: self class superclass].
	^super selectItem: aMenuItem event: anEvent