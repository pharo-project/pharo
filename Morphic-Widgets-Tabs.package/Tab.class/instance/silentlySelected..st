protocol
silentlySelected: aBoolean

	(state isSelectedMatchingTheState: aBoolean) 
		ifTrue: [ ^ self ].
	
	state := state newStateForSelected: aBoolean.
	aBoolean
		ifTrue: [ self showCloseIcon ].
	
	"No need to change here since the toolbar Ibelong to will do it"