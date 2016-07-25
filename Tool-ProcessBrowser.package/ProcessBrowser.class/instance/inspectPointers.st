process actions
inspectPointers
	| tc pointers |
	selectedProcess ifNil: [ ^ self ].
	tc := thisContext.
	pointers := selectedProcess pointersToExcept: { self processList. tc. self}.
	pointers ifEmpty: [ ^ self ].
	pointers inspectWithLabel: 'Objects pointing to ' , selectedProcess browserPrintString