bit manipulation
<< shiftAmount
	"left shift"
	
	shiftAmount < 0 ifTrue: [self error: 'negative arg'].
	^ self bitShift: shiftAmount