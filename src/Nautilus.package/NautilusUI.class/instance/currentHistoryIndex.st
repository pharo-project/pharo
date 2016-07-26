history behavior
currentHistoryIndex

	| index item |
	index := 1 min: self getHistoryList size.
	index = 0 ifTrue: [ ^ 0 ].
	item := (self getHistoryList at: index) key.
	self selectedClass = item selectedClass ifFalse: [ ^ 0 ].
	self selectedMethod ifNil: [ ^ 0 ].
	^ self selectedMethod selector = item selectedMethod
		ifTrue: [ index ]
		ifFalse: [ 0 ]