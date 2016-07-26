private - utilities
createLabelMorph: aString

	^aString asStringMorph 
		font: self labelFont emphasis: 0;
		color: textColor;
		yourself. 