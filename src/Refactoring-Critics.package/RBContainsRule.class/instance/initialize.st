initialization
initialize
	super initialize.
	self matchesAny: #(
			'(`@object detect: [:`each | | `@temps| `@.Statements] ifNone: [nil]) isNil'
			'(`@object detect: [:`each | | `@temps| `@.Statements] ifNone: [nil]) notNil'
			'(`@object detect: [:`each | | `@temps| `@.Statements] ifNone: [nil]) = nil'
			'(`@object detect: [:`each | | `@temps| `@.Statements] ifNone: [nil]) == nil'
			'(`@object detect: [:`each | | `@temps| `@.Statements] ifNone: [nil]) ~= nil'
			'(`@object detect: [:`each | | `@temps| `@.Statements] ifNone: [nil]) ~~ nil'
			'`@object detect: [:`each | | `@temps| `@.Statements] ifNone: [| `@temps1 | `@.Statements2. ^`@anything]' )