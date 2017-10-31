initialization
initialize
	super initialize.
	self matchesAny: #(
			'| `@temps | 
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
			`@.Statements2' )