callbacks
debugHighlightedText

	UIManager default defer: [ 
		textMorph textArea editor debugHighlight.
		textMorph shoutStyler style: textMorph text. ]
	