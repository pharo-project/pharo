execution
execute
	(NautilusRefactoring model: context)
				renameTemporaryNamed: context selectedVariableName
				Between: (context nodeStart to: context nodeStop)
				from: context selectedMethod