building
newContentMorph
	"Answer a new button morph"

	|b|
	b := (self theme
		newButtonIn: self
		for: self
		getState: nil
		action: #chooseColor
		arguments: #()
		getEnabled: #enabled
		label: (self newHatchMorph layoutInset: 2)
		help: nil)
		hResizing: #spaceFill.
	b contentHolder hResizing: #spaceFill.
	^b