as yet unclassified
borderColorToUse
	"Answer the border color to use based on the selection state."
	
	^self selected
		ifTrue: [self selectedBorderColor]
		ifFalse: [super borderColorToUse]