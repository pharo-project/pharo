fill-styles
dropListNormalFillStyleFor: aDropList
	"Return the normal fillStyle for the given drop list."
	
	|round roundCorners tl t tr l m r bl b br|
	round := aDropList wantsRoundedCorners.
	roundCorners := aDropList roundedCorners.
	tl := (round and: [roundCorners includes: 1]) ifTrue: [self buttonTopLeftForm] ifFalse: [self buttonSquareTopLeftForm].
	t := self buttonTopMiddleForm.
	tr:= (round and: [roundCorners includes: 4]) ifTrue: [self buttonTopRightForm] ifFalse: [self buttonSquareTopRightForm].
	l := self buttonMiddleLeftForm. m := self buttonPlainMiddleColor. r := self buttonMiddleRightForm.
	bl := (round and: [roundCorners includes: 2]) ifTrue: [self buttonBottomLeftForm] ifFalse: [self buttonSquareBottomLeftForm].
	b := self buttonBottomMiddleForm.
	br := (round and: [roundCorners includes: 3]) ifTrue: [self buttonBottomRightForm] ifFalse: [self buttonSquareBottomRightForm].
	^self
		multiFormFillStyleFrom: {tl. t. tr. l. m. r. bl. b. br}
		in: aDropList bounds