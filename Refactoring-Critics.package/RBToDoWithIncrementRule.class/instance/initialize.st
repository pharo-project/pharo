initialization
initialize
	super initialize.
	self matcher 
		matchesAnyOf: #(
			'`@i to: `@j do: [:`e | | `@temps | `@.Stmts. `x := `x + 1. `@.Stmts2]'
			'`@i to: `@j by: `@k do: [:`e | | `@temps | `@.Stmts. `x := `x + `@k. `@.Stmts2]' 
			'`@i to: `@j do: [:`e | | `@temps | `@.Stmts. `x := `x - 1. `@.Stmts2]'
			'`@i to: `@j by: `@k do: [:`e | | `@temps | `@.Stmts. `x := `x - `@k. `@.Stmts2]')
		do: [ :node :answer | node ]