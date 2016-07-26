fill-styles-buttons
buttonColouredFillStyleFor: aButton
	"Return the coloured button fillStyle for the given button."
	
	|round roundCorners tl t tr l m r bl b br|
	round := aButton wantsRoundedCorners.
	roundCorners := aButton roundedCorners.
	tl := (round and: [roundCorners includes: 1]) ifTrue: [self buttonSelectedTopLeftForm] ifFalse: [self buttonSquareSelectedTopLeftForm].
	t := self buttonSelectedTopMiddleForm.
	tr:= (round and: [roundCorners includes: 4]) ifTrue: [self buttonSelectedTopRightForm] ifFalse: [self buttonSquareSelectedTopRightForm].
	l := self buttonSelectedMiddleLeftForm. m := self buttonColouredMiddleColor. r := self buttonSelectedMiddleRightForm.
	bl := (round and: [roundCorners includes: 2]) ifTrue: [self buttonSelectedBottomLeftForm] ifFalse: [self buttonSquareSelectedBottomLeftForm].
	b := self buttonSelectedBottomMiddleForm.
	br := (round and: [roundCorners includes: 3]) ifTrue: [self buttonSelectedBottomRightForm] ifFalse: [self buttonSquareSelectedBottomRightForm].
	^self
		multiFormFillStyleFrom: {tl. t. tr. l. m. r. bl. b. br}
		in: aButton bounds