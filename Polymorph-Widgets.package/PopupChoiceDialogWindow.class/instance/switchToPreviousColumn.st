as yet unclassified
switchToPreviousColumn
	"Give the previous embedded menu keyboard focus."

	(self choiceMenus isNil or: [ self choiceMenus isEmpty ])
		ifTrue: [ ^ self ].
	self choiceMenus detect: [ :m | m hasKeyboardFocus ] ifFound: [ :menuWithFocus | menuWithFocus navigateFocusBackward ].
	self choiceMenus detect: [ :m | m hasKeyboardFocus ] ifNone: [ self choiceMenus last takeKeyboardFocus ]