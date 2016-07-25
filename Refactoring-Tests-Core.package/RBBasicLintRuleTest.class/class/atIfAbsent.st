intention revealing
atIfAbsent
	^self createParseTreeRule: 
				#('`@object 
						at: `@atArg 
						ifAbsent: [| `@temps | `@.Statements. `@object at: `@atArg put: `@putArg]' 
				'`@object 
						at: `@atArg 
						ifAbsent: [| `@temps | 
								`@.Statements. 
								`@object at: `@atArg put: `@putArg. 
								`@.xStatements1. 
								`@putArg]') 
		name: 'Uses at:ifAbsent: instead of at:ifAbsentPut:'