accessing
currentParagraphStyle
	"Answer the TextStyle for the current selection or cursor location if any"
	
	| i t |
	i := self startIndex.
	t := self text.
	^i > t size
		ifTrue: [ self text styleIfApplying: self emphasisHere ]
		ifFalse: [ self text textStyleAt: i ]