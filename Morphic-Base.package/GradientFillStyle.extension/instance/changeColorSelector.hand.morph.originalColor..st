*Morphic-Base-Balloon
changeColorSelector: aSymbol hand: aHand morph: aMorph originalColor: originalColor
	"Change either the firstColor or the lastColor (depending on aSymbol).  Put up a color picker to hande it.  We always use a modal picker so that the user can adjust both colors concurrently."
	|arraySelector|
	"A bad hack, I know... but it already uses a perform which depends on only firstColor: ... / lastColor: ... being sent"
	arraySelector := (aSymbol readStream upTo: $C) asSymbol.
(	UIManager default chooseColor: (colorRamp perform: arraySelector) value)
		ifNotNil: [:nc | 
			self 
				perform: aSymbol 
				with: nc 
				with: aMorph 
				with: aHand]

