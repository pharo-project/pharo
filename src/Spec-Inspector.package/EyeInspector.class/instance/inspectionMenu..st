menu
inspectionMenu: aMenu
	self hasSelectedObject ifFalse: [ ^ aMenu ].
	^ super inspectionMenu: aMenu.
	