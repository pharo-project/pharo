initialization
initialize
	| patterns |
	super initialize.
	patterns := OrderedCollection new.
	patterns addAll: (self selectors
		collect: [ :each | '`@object size > 0 ifTrue: [`@object' , (self genericPatternForSelector: each) , '. `@.Statements2]' ]).
	patterns addAll: (self selectors
		collect: [ :each | '`@object isEmpty ifFalse: [`@object' , (self genericPatternForSelector: each) , '. `@.Statements2]' ]).
	patterns addAll: (self selectors
		collect: [ :each | '`@object notEmpty ifTrue: [`@object' , (self genericPatternForSelector: each) , '. `@.Statements2]' ]).
	patterns addAll: (self selectors
		collect: [ :each | '`@object size = 0 ifFalse: [`@object' , (self genericPatternForSelector: each) , '. `@.Statements2]' ]).
	self matchesAny: patterns
		