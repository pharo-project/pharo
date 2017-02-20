menu
inspectionMenu: aMenu
	| menuModel |
	self hasSelectedObject ifFalse: [ ^ aMenu ].
	menuModel := MenuModel new.
	self inspectionSubMenu: menuModel.
	self selectedElement inspectionMenu: menuModel.
	aMenu addAllMorphs: (menuModel buildWithSpec: #popup) items.
	^aMenu