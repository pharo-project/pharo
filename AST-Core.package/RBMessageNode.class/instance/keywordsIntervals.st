accessing
keywordsIntervals
	^ selector keywords
		with: self keywordsPositions 
		collect: [ :keyword :start | start to: (start == 0 ifTrue: [ -1 ] ifFalse: [ start + keyword size - 1]) ].