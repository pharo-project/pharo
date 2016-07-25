deprecated
offerMenuFromEsc: aKeyboardEvent
	"The escape key was hit while the receiver has the keyboard focus; take action"
 	ActiveEvent shiftPressed 
		ifFalse: [ self raiseContextMenu: aKeyboardEvent].
	^ false 	