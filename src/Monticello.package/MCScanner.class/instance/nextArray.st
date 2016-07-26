actions
nextArray
	stream next. "("
	^ Array streamContents: [:s |
		[	stream skipSeparators.
			(stream peek = $)) or: [stream atEnd]
		] whileFalse: [ s nextPut: self next ].
		stream next = $) 
			ifFalse: [ self error: 'Unclosed array' ]]