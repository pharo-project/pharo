scripting-convenience
match: aBlockWithTwoArguments
	self filter: (GTFilterBlock new 
		matcher: aBlockWithTwoArguments;
		yourself)