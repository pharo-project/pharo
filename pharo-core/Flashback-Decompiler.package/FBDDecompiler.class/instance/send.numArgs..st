data flow instructions
send: selector numArgs: numberArguments
	| args |
	args := self popFromStack: numberArguments.
	simulatedStack addLast: (builder codeMessage: selector receiver: simulatedStack removeLast arguments: args)