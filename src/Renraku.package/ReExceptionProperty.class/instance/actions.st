actions
actions

	^ {RePropertyAction new
			icon: (self iconNamed: #smallDebug);
			description: 'Debug the exception';
			action: [ :prop |
				UIManager default
					debugProcess: Processor activeProcess 
					context: prop stack
					label: prop message 
					fullView: true ];
			yourself}