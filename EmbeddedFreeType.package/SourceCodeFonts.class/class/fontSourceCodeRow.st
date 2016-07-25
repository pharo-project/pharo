settings
fontSourceCodeRow
	| theme |
	
	theme := Smalltalk ui theme.
	^ theme newRowIn: World for: {
		theme buttonLabelForText: 'Predefined styles: ' translated.
		self fontButton: 'Small' size: self sizeSmall.
		self fontButton: 'Medium' size: self sizeMedium.
		self fontButton: 'Large' size: self sizeLarge.
		self fontButton: 'Very Large' size: self sizeVeryLarge.
		self fontButton: 'Huge' size: self sizeHuge	}	
		
		