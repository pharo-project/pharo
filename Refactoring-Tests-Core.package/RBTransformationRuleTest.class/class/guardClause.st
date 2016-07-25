transformations
guardClause
	^self
		rewrite: #(
			#('`@methodName: `@args 
					| `@temps | 
					`@.Statements. 
					`@condition ifTrue: [| `@trueTemps | `.Statement1. `.Statement2. `@.Statements1]'
			"->"
			'`@methodName: `@args
					| `@temps `@trueTemps |
					`@.Statements.
					`@condition ifFalse: [^self].
					`.Statement1.
					`.Statement2.
					`@.Statements1') 
			#('`@methodName: `@args 
					| `@temps | 
					`@.Statements. 
					`@condition ifFalse: [| `@falseTemps | `.Statement1. `.Statement2. `@.Statements1]'
			"->"
			'`@methodName: `@args
					| `@temps `@falseTemps |
					`@.Statements.
					`@condition ifTrue: [^self].
					`.Statement1.
					`.Statement2.
					`@.Statements1'))
		methods: true
		name: 'Eliminate guarding clauses'