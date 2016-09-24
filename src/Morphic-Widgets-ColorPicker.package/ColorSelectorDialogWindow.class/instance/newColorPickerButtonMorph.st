actions
newColorPickerButtonMorph
	"Answer a button to enable picking of colour."

	^ self
		newButtonFor: self
		getState: nil
		action: #pickColor
		arguments: nil
		getEnabled: nil
		labelForm:
			((self iconNamed: #eyedropperIcon)
				scaledIntoFormOfSize: 16)
		help: 'Pick a color from the screen' translated