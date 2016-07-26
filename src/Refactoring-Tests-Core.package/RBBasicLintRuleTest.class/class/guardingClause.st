intention revealing
guardingClause
	^self
		createParseTreeRule: 
				#('`@MethodName: `@args 
						| `@temps | 
						`@.Statements. 
						`@condition ifTrue: [| `@BlockTemps | `.Statement1. `.Statement2. `@.BStatements]' 
				'`@MethodName: `@args 
						| `@temps | 
						`@.Statements. 
						`@condition ifFalse: [| `@BlockTemps | `.Statement1. `.Statement2. `@.BStatements]')
		method: true
		name: 'Guarding clauses'