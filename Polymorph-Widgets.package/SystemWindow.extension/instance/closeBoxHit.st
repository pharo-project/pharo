*Polymorph-Widgets
closeBoxHit
	"The user clicked on the close-box control in the window title.
	For Mac users only, the Mac convention of option-click-on-close-box is obeyed if the mac option key is down.
	If we have a modal child then don't delete.
	Play the close sound now since this is the only time we know that the close is user-initiated."

	self allowedToClose ifFalse: [^self].
	self playCloseSound.
	self close
