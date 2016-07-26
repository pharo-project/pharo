initialization
initialize
	super initialize.
	self rewriteRule
		replace: '``@collection contains: [:`each | | `@temps | ``@.Statements]'
		with: '``@collection anySatisfy: [:`each | | `@temps | ``@.Statements]';
		
		replace: '(``@collection detect: [:`each | | `@temps | ``@.Statements] ifNone: [nil]) isNil'
		with: '(``@collection anySatisfy: [:`each | | `@temps | ``@.Statements]) not';
		
		replace: '(``@collection detect: [:`each | | `@temps | ``@.Statements] ifNone: [nil]) = nil'
		with: '(``@collection anySatisfy: [:`each | | `@temps | ``@.Statements]) not';
		
		replace: '(``@collection detect: [:`each | | `@temps | ``@.Statements] ifNone: [nil]) == nil'
		with: '(``@collection anySatisfy: [:`each | | `@temps | ``@.Statements]) not';
		
		replace: '(``@collection detect: [:`each | | `@temps | ``@.Statements] ifNone: [nil]) notNil'
		with: '``@collection anySatisfy: [:`each | | `@temps | ``@.Statements]';
		
		replace: '(``@collection detect: [:`each | | `@temps | ``@.Statements] ifNone: [nil]) ~= nil'
		with: '``@collection anySatisfy: [:`each | | `@temps | ``@.Statements]';
		
		replace: '(``@collection detect: [:`each | | `@temps | ``@.Statements] ifNone: [nil]) ~~ nil'
		with: '``@collection anySatisfy: [:`each | | `@temps | ``@.Statements]'