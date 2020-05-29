Extension { #name : #SystemWindow }

{ #category : #'*Morphic-Widgets-Windows' }
SystemWindow >> openModal: aSystemWindow [
	"Open the given window locking the receiver until it is dismissed.
	Answer the system window.
	Restore the original keyboard focus when closed."

	| keyboardFocus |
	
	keyboardFocus := self activeHand keyboardFocus.
	self modalLockTo: aSystemWindow.
	[ aSystemWindow openModal ] ensure: [
		self modalUnlockFrom: aSystemWindow.
		self activeHand newKeyboardFocus: keyboardFocus ].
	^aSystemWindow
]
