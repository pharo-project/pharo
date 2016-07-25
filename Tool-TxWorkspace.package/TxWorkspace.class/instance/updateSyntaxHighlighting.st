syntax highlighting
updateSyntaxHighlighting
	"don't try it at home"
	| textEditor start end  |
	
	textEditor := editor controller.	
	text := textEditor text.
		
	self flag: 'styling changes the text, and we forced to use crappy method of preserving selection..'.
	start := textEditor selection start asTextIndex.
	end := textEditor selection end asTextIndex.
	
	styler restyle: text inContext: nil isMethod: false.

	"restore selection"	
	textEditor selection start: (text positionAtTextIndex: start).
	textEditor selection end: (text positionAtTextIndex: end).

