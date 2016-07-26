intention revealing
whileTrue
	^self createParseTreeRule: 
			#('| `@temps | 
				`@.Statements1. 
				[`index <= `@stop] 
					whileTrue: 
						[| `@blockTemps | 
						`@.BlockStmts1. 
						`index := `index + 1].
				`@.Statements2' 
			'| `@temps | 
				`@.Statements1. 
				[`index < `@stop] 
					whileTrue: 
						[| `@blockTemps | 
						`@.BlockStmts1. 
						`index := `index + 1].
				`@.Statements2'
			'| `@temps | 
				`@.Statements1. 
				[`index >= `@stop] 
					whileTrue: 
						[| `@blockTemps | 
						`@.BlockStmts1. 
						`index := `index - 1].
				`@.Statements2' 
			'| `@temps | 
				`@.Statements1. 
				[`index > `@stop] 
					whileTrue: 
						[| `@blockTemps | 
						`@.BlockStmts1. 
						`index := `index - 1].
				`@.Statements2') 
		name: 'Uses whileTrue: instead of to:do:'