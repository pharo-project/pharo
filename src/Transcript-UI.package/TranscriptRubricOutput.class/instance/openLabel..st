window
openLabel: aString
	
	| window |
	
	textMorph := RubScrolledTextModel new newScrolledText.
	
	textMorph
		beWrapped; 	
		withLineNumbers; 
		withoutAdornment;
		beForSmalltalkScripting.
	
	"for the menu to work..."
	textMorph textArea model interactionModel: TranscriptRubricInteractionModel new.
	
	textMorph textArea withoutShoutStyler.
	
	window := (SystemWindow labelled: aString) model: self.
	
	window addMorph: textMorph frame: (0 @ 0 corner: 1 @ 1).
	
	^window openInWorld