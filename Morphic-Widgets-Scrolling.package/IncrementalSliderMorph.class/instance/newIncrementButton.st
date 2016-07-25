building
newIncrementButton
	"Answer a new increment button."

	^(self theme builder
		newButtonFor: self
		action: #increment
		getEnabled: #maxEnabled
		label: (self newButtonLabel: #right ofSize: 24)
		help: nil)
		vResizing: #spaceFill;
		width: 64;
		roundedCorners: #(3 4);
		setProperty: #wantsKeyboardFocusNavigation toValue: false;
		on: #mouseStillDown send: #increment to: self