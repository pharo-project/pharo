commands
send: aSelector withArguments: arguments


	self commands add: (SpecLayoutSend
							selector: aSelector
							arguments: arguments).