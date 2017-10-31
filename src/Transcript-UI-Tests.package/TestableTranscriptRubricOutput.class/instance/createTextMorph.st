accessing
createTextMorph

	textMorph := RubScrolledTextModel new newScrolledText.
	
	textMorph 
		beWrapped; 	
		withLineNumbers; 
		withoutAdornment;
		beForSmalltalkScripting.
	
	textMorph textArea withoutShoutStyler.