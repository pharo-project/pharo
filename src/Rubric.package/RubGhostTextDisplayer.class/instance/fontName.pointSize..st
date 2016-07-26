accessing
fontName: aFontName pointSize: anInteger
	self
		handleTextChangeWhile: [ 
			ghostText
				beEditableWhile: [ 
					ghostText fontName: aFontName pointSize: anInteger.
					ghostText compose.
					self changed ] ]