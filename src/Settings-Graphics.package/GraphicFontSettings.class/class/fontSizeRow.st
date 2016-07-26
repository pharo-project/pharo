fonts
fontSizeRow
	^ self theme newRowIn: World for: {
			self theme buttonLabelForText: 'Predefined styles: ' translated.
			self fontChoiceButtonForStyle: #small label: 'Small' translated.
			self fontChoiceButtonForStyle: #medium label: 'Medium' translated.
			self fontChoiceButtonForStyle: #large label: 'Large' translated.
			self fontChoiceButtonForStyle: #veryLarge label: 'Very large' translated.
			self fontChoiceButtonForStyle: #huge label: 'Huge' translated.
	}