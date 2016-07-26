protocol
selected: aBoolean

	(state isSelectedMatchingTheState: aBoolean) 
		ifTrue: [ ^ self triggerEvent: #tabResetSelection with: self ].
	
	state := state newStateForSelected: aBoolean.
	aBoolean
		ifTrue: [ 
			self triggerEvent: #tabSelected with: self.
			self showCloseIcon ].
	
	"No need to change here since the toolbar Ibelong to will do it"