building
newDecrementButton
	"Answer a new decrement button."

	^(self theme builder
		newButtonFor: self
		action: #decrement
		getEnabled: #minEnabled
		label: (self newButtonLabel: #left ofSize: 24)
		help: nil)
		vResizing: #spaceFill;
		width: 64;
		roundedCorners: #(1 2);
		setProperty: #wantsKeyboardFocusNavigation toValue: false;
		on: #mouseStillDown send: #decrement to: self