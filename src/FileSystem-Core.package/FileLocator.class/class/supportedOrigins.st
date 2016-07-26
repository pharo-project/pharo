accessing
supportedOrigins
	| origins current |
	origins := IdentitySet new.
	current := Resolver.
	[current notNil] whileTrue:
		[origins addAll: current supportedOrigins.
		current := current next].
	^ origins