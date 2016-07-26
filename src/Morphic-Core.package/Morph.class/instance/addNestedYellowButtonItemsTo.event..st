menu
addNestedYellowButtonItemsTo: aMenu event: evt
	"Add items to aMenu starting with me and proceeding down 
	through my submorph chain, 
	letting any submorphs that include the event position 
	contribute their items to the bottom of the menu, separated by 
	a line."

	| underMouse submenu |
	self addYellowButtonMenuItemsTo: aMenu event: evt.
	underMouse := self submorphThat: [ :each | each containsPoint: evt position ] ifNone: [ ^ self ].
	(underMouse addMyYellowButtonMenuItemsToSubmorphMenus and: [ underMouse hasYellowButtonMenu ])
		ifFalse: [ ^ self ].
	aMenu addLine.
	submenu := UIManager default newMenuIn: underMouse for: underMouse.
	underMouse addNestedYellowButtonItemsTo: submenu event: evt.
	aMenu add: underMouse externalName icon: (underMouse iconOrThumbnailOfSize: 16) subMenu: submenu