menu
inspectionMenu: aMenu
	| menuModel |
	self hasSelectedObject ifFalse: [ ^ aMenu ].
	menuModel := MenuPresenter new.
	self inspectionSubMenu: menuModel.
	self selectedElement inspectionMenu: menuModel.
	aMenu addAllMorphs: (menuModel buildWithSpec: #popup) items.
	^aMenu