initialization
initialize
	super initialize.
	self addMatchingMethod:
		'`@message: `@args
			| `@temps |
			`@.before.
			self `@message: `@args.
			`@.after'
		