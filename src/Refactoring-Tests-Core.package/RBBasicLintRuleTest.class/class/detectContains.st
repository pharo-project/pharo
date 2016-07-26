intention revealing
detectContains
	^self createParseTreeRule: 
			#('`@collection do: [:`each | | `@temps | 
					`@.Statements1. 
					`@condition ifFalse: [| `@BlockTemps | `@.BlockStatements1. ^`each]. 
					`@.Statements2]' 
			'`@collection do: [:`each | | `@temps | 
					`@.Statements1. 
					`@condition ifTrue: [| `@BlockTemps | `@.BlockStatements1.  ^`each]. 
					`@.Statements2]' 
			'`@collection do: [:`each | | `@temps | 
					`@.Statements1. 
					`@condition ifFalse: [| `@BlockTemps | `@.BlockStatements1. ^true]. 
					`@.Statements2]' 
			'`@Collection do: [:`each | | `@temps | 
					`@.Statements1. 
					`@condition ifTrue: [| `@BlockTemps | `@.BlockStatements1.  ^true]. 
					`@.Statements2]' 
			'`@collection do: [:`each | | `@temps | 
					`@.Statements1. 
					`@condition ifFalse: [| `@BlockTemps | `@.BlockStatements1. ^false]. 
					`@.Statements2]' 
			'`@collection do: [:`each | | `@temps | 
					`@.Statements1. 
					`@condition ifTrue: [| `@BlockTemps | `@.BlockStatements1.  ^false]. 
					`@.Statements2]') 
		name: 'Uses do: instead of contains: or detect:''s'