accessing
lastParagraphStyle
	"Answer the TextStyle for to be used if positioned at the end of the text"
	
	| t |
	t := self text.
	^self startIndex > t size
		ifTrue: [ self text styleIfApplying: self emphasisHere ]
		ifFalse: [ self text textStyleAt: t size +1 ]