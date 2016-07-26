accessing
pointIndex
	^ self pointBlock ifNotNil: [:p | p stringIndex ] ifNil: [ 0 ]