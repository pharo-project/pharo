accessing-menu
errorMenu: aMenu
	"Add the browse item only if an item is selected"

	self statusMenu: aMenu.
	errorSelected ifNotNil: [aMenu add: 'Browse test' selector: #browseSelectedErrorTest].
	^aMenu