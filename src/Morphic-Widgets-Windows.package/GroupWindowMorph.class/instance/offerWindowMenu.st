windows
offerWindowMenu
	"Popup the window menu. Fill from current workspace."

	| aMenu |
	aMenu := self buildWindowMenu.
	aMenu
		addLine;
		add: 'Grab window...' target: self selector: #grabWindow.
	aMenu lastItem icon: (Smalltalk ui icons iconNamed: #smallWindowIcon).
	self tabGroup page
		ifNotNil: [ :page | page model addModelItemsToWindowMenu: aMenu ].
	aMenu popUpEvent: self currentEvent in: self world