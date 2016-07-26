rendering
renderOn: renderer
	color isTransparent ifFalse: [
		renderer renderBackground: self
	]