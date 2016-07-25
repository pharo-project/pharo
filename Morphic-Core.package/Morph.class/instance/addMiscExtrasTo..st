menus
addMiscExtrasTo: aMenu
	"Add a submenu of miscellaneous extra items to the menu."

	| subMenu |
	self isWorldMorph
		ifTrue: [ ^ self ].
	subMenu := UIManager default newMenuIn: self for: self.
	self renderedMorph isSystemWindow
		ifFalse: [ subMenu add: 'put in a window' selector: #embedInWindow ].
	subMenu add: 'adhere to edge...' selector: #adhereToEdge.
	aMenu add: 'extras...' subMenu: subMenu