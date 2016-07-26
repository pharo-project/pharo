possible bugs
returnsIfTrue
	^self createParseTreeRule: 
				#('^`@condition ifTrue: [| `@temps | `@.statements]' 
				'^`@condition ifFalse: [| `@temps | `@.statements]') 
		name: 'Returns value of ifTrue:/ifFalse: without ifFalse:/ifTrue: block'