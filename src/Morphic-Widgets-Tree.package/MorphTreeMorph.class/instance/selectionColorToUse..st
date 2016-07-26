selection
selectionColorToUse: aColor
	"Set the colour for selected items."

	aColor = self selectionColorToUse ifTrue: [^self].
	aColor
		ifNil: [self removeProperty: #selectionColorToUse]
		ifNotNil: [self setProperty: #selectionColorToUse toValue: aColor].
