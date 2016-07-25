initialization
initialize
	super initialize.
	self matcher 
		matchesAnyOf: #(
			'`@cursor showWhile: [| `@temps | `@.Statements1. `var := `@object]'
			'`@cursor showWhile: [| `@temps | `@.Statements1. ^`@object]'
			'[| `@temps | `@.Statements. `var := `@object] ensure: `@block'
			'[| `@temps | `@.Statements. ^`@object] ensure: `@block'
			'[| `@temps | `@.Statements. `var := `@object] ifCurtailed: `@block'
			'[| `@temps | `@.Statements. ^`@object] ifCurtailed: `@block' )
		do: [ :node :answer | node ]