accessing
-- aPosition
	"Answer the distance between receiver and position, in terms of number of moving receiver right (positive)
	or left (negative) until it meets a given position. Answer zero if two positions coincide"
	
	(self hasSamePositionAs: aPosition) ifTrue: [  ^ 0 ].
	
	^ self < aPosition 
		ifTrue: [ self timesToMoveRightTo: aPosition ]
		ifFalse: [ (aPosition timesToMoveRightTo: self) negated ]