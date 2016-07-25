initialization
initialize
	super initialize.
	self matcher
		matchesAnyOf: #(
			'`@object 
				ifTrue: [| `@temps1 | `@.Statements1. `.Statement] 
				ifFalse: [| `@temps2 | `@.Statements2. `.Statement]' 
			'`@object 
				ifTrue: [| `@temps1 | `.Statement. `@.Statements1] 
				ifFalse: [| `@temps2 | `.Statement. `@.Statements2]' 
			'`@object 
				ifFalse: [| `@temps1 | `@.Statements1. `.Statement] 
				ifTrue: [| `@temps2 | `@.Statements2. `.Statement]' 
			'`@object 
				ifFalse: [| `@temps1 | `.Statement. `@.Statements1] 
				ifTrue: [| `@temps2 | `.Statement. `@.Statement2]') 
			do: [ :node :answer | 
				answer isNil 
					ifTrue: [
						| statement |
						statement := node arguments first body statements last.
						(statement isVariable and: [ statement = node arguments last body statements last ]) 
							ifFalse: [ node ]
							ifTrue: [ nil ] ]
					ifFalse: [ answer ] ]