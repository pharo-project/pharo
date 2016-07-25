private
pvtCritical: aBlock startingAt: index
	| mutex |
	index > array size ifTrue:[^aBlock value].
	mutex := array at: index.
	^mutex critical:[self pvtCritical: aBlock startingAt: index+1].