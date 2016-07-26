private
spaceUsed
	^super spaceUsed + (self hasClassSide
		ifTrue: [self classSide spaceUsed] 
		ifFalse: [0])