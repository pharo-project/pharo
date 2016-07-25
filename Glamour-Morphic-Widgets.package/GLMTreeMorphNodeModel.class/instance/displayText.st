accessing
displayText
	^ (UITheme current 
		newTextIn: World
		text: (self containerTree glamourPresentation formatedDisplayValueOf: self item))
			backgroundColor: self textBackgroundColor;
			yourself