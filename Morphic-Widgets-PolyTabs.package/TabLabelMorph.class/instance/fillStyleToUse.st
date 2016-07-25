accessing
fillStyleToUse
	"Answer the basic fill style for the receiver."

	^self isSelected
		ifTrue: [self selectedFillStyle] 
		ifFalse: [self normalFillStyle]