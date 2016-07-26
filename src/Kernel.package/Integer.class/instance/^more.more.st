bit manipulation
>> shiftAmount
	"right shift"
	
	shiftAmount < 0 ifTrue: [self error: 'negative arg'].
	^ self bitShift: 0 - shiftAmount