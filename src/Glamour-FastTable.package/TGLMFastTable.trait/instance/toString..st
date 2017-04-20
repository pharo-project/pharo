instance creation
toString: anObject
	^ self glamourPresentation hasFilterBlock
		ifTrue: [ anObject ]
		ifFalse: [ super toString: anObject ]