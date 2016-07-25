morphic protocol
listIndex: aNumber
	listIndex := aNumber.
	self answer: (list at: listIndex ifAbsent: [ nil ]).
	self changed: #listIndex.