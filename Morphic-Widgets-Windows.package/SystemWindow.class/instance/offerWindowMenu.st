menu actions
offerWindowMenu
	| aMenu |
	aMenu := self buildWindowMenu.
	model ifNotNil:
		[model addModelItemsToWindowMenu: aMenu].
	aMenu popUpEvent: self currentEvent in: self world