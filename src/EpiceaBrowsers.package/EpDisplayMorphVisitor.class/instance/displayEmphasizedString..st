private
displayEmphasizedString: aString 

	^ { aString asMorph
			color: self textColor;
			emphasis: 1;
			yourself }