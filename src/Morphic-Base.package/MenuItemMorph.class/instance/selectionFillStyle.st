private
selectionFillStyle
	"answer the fill style to use with the receiver is the selected  
	element"
	Display depth <= 2 ifTrue: [^ Color gray].
	^ self theme settings menuSelectionColor.
