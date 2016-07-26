*Morphic-Base
changeBorderColor: evt
	self theme
		chooseColorIn: self
		title: 'Choose Color' translated
		color: self borderStyle color
		for: [ :newColor | self borderColor: newColor ]