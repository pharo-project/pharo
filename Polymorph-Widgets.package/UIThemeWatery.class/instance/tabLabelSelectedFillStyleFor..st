fill-styles-buttons
tabLabelSelectedFillStyleFor: aTabLabel
	"Return the selected fillStyle for the given tab label."
	
	|round roundCorners tl t tr l m r|
	round := aTabLabel wantsRoundedCorners.
	roundCorners := aTabLabel roundedCorners.
	tl := (round and: [roundCorners includes: 1]) ifTrue: [self buttonSelectedTopLeftForm] ifFalse: [self buttonSquareSelectedTopLeftForm].
	t := self buttonSelectedTopMiddleForm.
	tr := (round and: [roundCorners includes: 4]) ifTrue: [self buttonSelectedTopRightForm] ifFalse: [self buttonSquareSelectedTopRightForm].
	l := self buttonSelectedMiddleLeftForm. m := self buttonColouredMiddleColor. r := self buttonSelectedMiddleRightForm.
	^self
		multiFormTopFillStyleFrom: {tl. t. tr. l. m. r}
		in: aTabLabel bounds