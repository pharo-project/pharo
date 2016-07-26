private
restoreFocus: originalFocusHolder in: aWorld
	"restore only if current focus holder is nil "

	aWorld primaryHand keyboardFocus ifNotNil: [ ^ self ].
	originalFocusHolder ifNotNil: [ aWorld primaryHand newKeyboardFocus: originalFocusHolder ]