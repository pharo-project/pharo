as yet unclassified
switchToNextColumn
	"Give the next embedded menu keyboard focus."

	(self choiceMenus isNil or: [ self choiceMenus isEmpty ])
		ifTrue: [ ^ self ].
	self choiceMenus detect: [ :m | m hasKeyboardFocus ] ifFound: [ :menuWithFocus | menuWithFocus navigateFocusForward ].
	self choiceMenus detect: [ :m | m hasKeyboardFocus ] ifNone: [ self choiceMenus first takeKeyboardFocus ]