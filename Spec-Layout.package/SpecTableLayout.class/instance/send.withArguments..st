commands
send: aSelector withArguments: arguments
	commands 
		add: (SpecTableLayoutSend
					selector: aSelector
					arguments: arguments)