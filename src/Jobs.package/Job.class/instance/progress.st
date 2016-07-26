progress
progress
	"Avoid negative progress and divideByZero."
	^ min >= max ifTrue: [ 1 ] ifFalse: [ (currentValue - min) / (max - min) ]