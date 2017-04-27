menu
menuMorphForSelectedItems

	| aMenu |
	aMenu := MenuModel new.

	showEntryItemMenu ifTrue: [ self addMenuItemsForSelectedItemsIn:	aMenu ].

	^ aMenu buildWithSpecAsPopup
