initialization
initialize
	super initialize.
	self matcher 
		matchesAnyMethodOf: #(
			'`@MethodName: `@args 
				| `@temps | 
				`@.Statements. 
				`@condition ifTrue: [| `@BlockTemps | `.Statement1. `.Statement2. `@.BStatements]' 
			'`@MethodName: `@args 
				| `@temps | 
				`@.Statements. 
				`@condition ifFalse: [| `@BlockTemps | `.Statement1. `.Statement2. `@.BStatements]' )
		do: [ :node :answer | 
			answer isNil 
				ifTrue: [ node body statements last ]
				ifFalse: [ answer ] ]