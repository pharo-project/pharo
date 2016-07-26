drawing-general
roundCornersOf: aMorph in: bounds during: aBlock
	aMorph wantsRoundedCorners ifFalse:[^aBlock value].
	(self seesNothingOutside: (CornerRounder rectWithinCornersOf: bounds))
		ifTrue: ["Don't bother with corner logic if the region is inside them"
				^ aBlock value].
	CornerRounder roundCornersOf: aMorph on: self in: bounds
		displayBlock: aBlock
		borderWidth: aMorph borderWidthForRounding
		corners: aMorph roundedCorners