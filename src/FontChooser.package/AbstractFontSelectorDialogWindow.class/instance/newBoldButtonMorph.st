instance creation
newBoldButtonMorph
	"Answer a button for the boldness of the font."

	^ self
		newButtonFor: self
		getState: #isBold
		action: #toggleBold
		arguments: nil
		getEnabled: nil
		labelForm: (self iconNamed: #smallBoldIcon)
		help: 'Toggle bold font' translated