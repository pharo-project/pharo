accessing
displayText
	^ UITheme current 
		newTextIn: World
		text: (self item presentations titleValue ifNil: ['noname']) 