process list
findContext
	| initialProcessIndex initialStackIndex found |
	initialProcessIndex := self processListIndex.
	initialStackIndex := self stackListIndex.
	searchString := UIManager default 
			request: 'Enter a string to search for in the process stack lists'
	  initialAnswer: searchString.
	searchString isEmptyOrNil
		ifTrue: [^ false].
	self processListIndex: 1.
	self stackListIndex: 1.
	found := self nextContext.
	found
		ifFalse: [self processListIndex: initialProcessIndex.
			self stackListIndex: initialStackIndex].
	^ found