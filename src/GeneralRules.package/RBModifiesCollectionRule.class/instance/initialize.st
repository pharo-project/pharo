initialization
initialize
	super initialize.
	self 
		matchesAny: #(
			'`@collection do: [:`each | | `@temps | `@.Statements]' 
			'`@collection collect: [:`each | | `@temps | `@.Statements]' 
			'`@collection select: [:`each | | `@temps | `@.Statements]' 
			'`@collection reject: [:`each | | `@temps | `@.Statements]' 
			'`@collection inject: `@value into: [:`sum :`each | | `@temps | `@.Statements]')
		