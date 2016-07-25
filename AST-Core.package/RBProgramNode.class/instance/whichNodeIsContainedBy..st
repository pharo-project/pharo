querying
whichNodeIsContainedBy: anInterval 
	| selectedChildren |
	(self intersectsInterval: anInterval) ifFalse: [^nil].
	(self containedBy: anInterval) ifTrue: [^self].
	selectedChildren := self children 
				select: [:each | each intersectsInterval: anInterval].
	^selectedChildren size == 1 
		ifTrue: [selectedChildren first whichNodeIsContainedBy: anInterval]
		ifFalse: [nil]