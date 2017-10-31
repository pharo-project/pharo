initialization
initialize
	super initialize.
	self
	
		" allSatisfy: "
		addMatchingMethod: '`@method: `@args
			| `@temps |
			`@.statements.
			`@collection do: [ :`each |
				| `@blocktemps |
				`@.blockstatements.
				`@condition
					ifFalse: [ ^ false ] ].
			^ true'
		rewriteTo: '`@method: `@args
			| `@temps |
			`@.statements. 
			^ `@collection allSatisfy: [ :`each |
				| `@blocktemps |
				`@.blockstatements.
				`@condition ]';

		" anySatisfy: "
		addMatchingMethod: '`@method: `@args
			| `@temps |
			`@.statements.
			`@collection do: [ :`each |
				| `@blocktemps |
				`@.blockstatements.
				`@condition
					ifTrue: [ ^ true ] ].
			^ false'
		rewriteTo: '`@method: `@args
			| `@temps |
			`@.statements.
			^ `@collection anySatisfy: [ :`each | 
				| `@blocktemps |
				`@.blockstatements.
				`@condition ]';

		" noneSatisfy: "
		addMatchingMethod: '`@method: `@args
			| `@temps |
			`@.statements.
			`@collection do: [ :`each |
				| `@blocktemps |
				`@.blockstatements.
				`@condition
					ifTrue: [ ^ false ] ].
			^ true'
		rewriteTo: '`@method: `@args
			| `@temps |
			`@.statements.
			^ `@collection noneSatisfy: [ :`each | 
				| `@blocktemps |
				`@.blockstatements.				
				`@condition ]'