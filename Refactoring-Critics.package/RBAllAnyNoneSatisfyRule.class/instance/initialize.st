initialization
initialize
	super initialize.
	self rewriteRule
	
		" allSatisfy: "
		replaceMethod: '`@method: `@args
			| `@temps |
			`@.statements.
			`@collection do: [ :`each |
				| `@blocktemps |
				`@.blockstatements.
				`@condition
					ifFalse: [ ^ false ] ].
			^ true'
		with: '`@method: `@args
			| `@temps |
			`@.statements. 
			^ `@collection allSatisfy: [ :`each |
				| `@blocktemps |
				`@.blockstatements.
				`@condition ]';

		" anySatisfy: "
		replaceMethod: '`@method: `@args
			| `@temps |
			`@.statements.
			`@collection do: [ :`each |
				| `@blocktemps |
				`@.blockstatements.
				`@condition
					ifTrue: [ ^ true ] ].
			^ false'
		with: '`@method: `@args
			| `@temps |
			`@.statements.
			^ `@collection anySatisfy: [ :`each | 
				| `@blocktemps |
				`@.blockstatements.
				`@condition ]';

		" noneSatisfy: "
		replaceMethod: '`@method: `@args
			| `@temps |
			`@.statements.
			`@collection do: [ :`each |
				| `@blocktemps |
				`@.blockstatements.
				`@condition
					ifTrue: [ ^ false ] ].
			^ true'
		with: '`@method: `@args
			| `@temps |
			`@.statements.
			^ `@collection noneSatisfy: [ :`each | 
				| `@blocktemps |
				`@.blockstatements.				
				`@condition ]'