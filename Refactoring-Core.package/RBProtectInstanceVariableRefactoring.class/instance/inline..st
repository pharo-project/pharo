transforming
inline: aSelector 
	self onError: 
			[self performComponentRefactoring: (RBInlineAllSendersRefactoring 
						model: self model
						sendersOf: aSelector
						in: class)]
		do: []