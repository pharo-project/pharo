menu
inspectionMenu: aMenu
	self hasSelectedObject ifFalse: [ ^ aMenu ].
	self inspectionSubMenu: aMenu.
	self selectedElement inspectionMenu: aMenu.
	^aMenu.