meta-actions
changeColorTarget: anObject selector: aSymbol originalColor: aColor hand: aHand
	"Put up a color picker for changing some kind of color."
	
	(UIManager default chooseColor: aColor)
		ifNotNil: [:nc | anObject perform: aSymbol with: nc].
