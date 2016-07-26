error handing
numArgsError: numArgsForInvocation

	| printNArgs |
	printNArgs := [:n| n printString, ' argument', (n = 1 ifTrue: [''] ifFalse:['s'])]. 
	self error: 
			'This block accepts ', (printNArgs value: numArgs), 
			', but was called with ', (printNArgs value: numArgsForInvocation), '.'