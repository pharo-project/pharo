private
currentRange
	^ ranges detect: [ :each | position between: each start and: each end ] ifNone: [ nil ]