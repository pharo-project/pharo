fill-styles-buttons
tabLabelNormalFillStyleFor: aTabLabel
	"Return the normal fillStyle for the given tab label."
	
	|round roundCorners tl t tr l m r|
	round := aTabLabel wantsRoundedCorners.
	roundCorners := aTabLabel roundedCorners.
	tl := (round and: [roundCorners includes: 1]) ifTrue: [self buttonTopLeftForm] ifFalse: [self buttonSquareTopLeftForm].
	t := self buttonTopMiddleForm.
	tr := (round and: [roundCorners includes: 4]) ifTrue: [self buttonTopRightForm] ifFalse: [self buttonSquareTopRightForm].
	l := self buttonMiddleLeftForm. m := self buttonPlainMiddleColor. r := self buttonMiddleRightForm.
	^self
		multiFormTopFillStyleFrom: {tl. t. tr. l. m. r}
		in: aTabLabel bounds