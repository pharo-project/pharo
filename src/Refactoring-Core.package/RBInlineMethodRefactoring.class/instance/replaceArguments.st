transforming
replaceArguments
	sourceMessage arguments reversed
		with: inlineParseTree arguments reversed
		do: [ :replacement :source | 
			(replacement isImmediateNode or: [ self shouldInlineExpression: replacement newSource ]) 
				ifTrue: [ self replaceArgument: source with: replacement ]
				ifFalse: [ self addTemporary: source assignedTo: replacement ] ]