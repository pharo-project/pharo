data flow instructions
superSend: selector numArgs: numberArguments
	| args |
	args := self popFromStack: numberArguments.
	simulatedStack removeLast.
	simulatedStack addLast: (builder codeMessage: selector receiver: builder codeSuper arguments: args ).