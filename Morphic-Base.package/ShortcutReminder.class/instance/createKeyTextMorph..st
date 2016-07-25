private - utilities
createKeyTextMorph: aString

	^(self fixKeyText: aString) asStringMorph 
		font: self keyTextFont emphasis: 2;
		color: textColor;
		yourself. 