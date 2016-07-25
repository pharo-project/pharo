intention revealing
collectionProtocol
	^self createParseTreeRule: 
			#('`@collection do: [:`each | | `@temps | `@.Statements1. `@object add: `@arg. `@.Statements2]' 
			'`@collection do: [:`each | | `@temps | 
					`@.Statements1. 
					`@condition ifTrue: [| `@blockTemps | 
							`@.BlockStatements1. 
							`@object add: `each. 
							`@.BlockStatements2]. 
					`@.Statements2]' 
			'`@collection do: [:`each | | `@temps | 
					`@.Statements1. 
					`@condition ifFalse: [| `@blockTemps | 
							`@.BlockStatements1. 
							`@object add: `each. 
							`@.BlockStatements2]. 
					`@.Statements2]') 
		name: 'Uses do: instead of collect: or select:''s'