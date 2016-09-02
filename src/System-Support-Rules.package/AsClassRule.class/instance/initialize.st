initialization
initialize
	super initialize.
	self
		replace: '`@expr asClass'
		   with: 'self class environment at: `@expr';
		
		replace: '`@expr asClassIfAbsent: `@block'
		with: 'self class environment at: `@expr ifAbsent: `@block';
		
	replace: '`@expr asClassIfPresent: `@block'
		with: 'self class environment at: `@expr ifPresent: `@block'