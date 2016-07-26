initialization
initialize
	super initialize.
	self matcher 
		matchesAnyOf: #(
			'`@collection do: [:`each | | `@temps | `@.Statements1. `@object add: `@arg. `@.Statements2]'
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
				`@.Statements2]' )
		do: [ :node :answer | node ]