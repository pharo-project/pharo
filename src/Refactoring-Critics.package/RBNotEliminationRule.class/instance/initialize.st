initialization
initialize
	super initialize.
	self rewriteRule
		replace: '``@object not not' with: '``@object';
		replace: '``@object isEmpty not' with: '``@object isNotEmpty';
		replace: '``@object not ifTrue: ``@block' with: '``@object ifFalse: ``@block';
		replace: '``@object not ifFalse: ``@block' with: '``@object ifTrue: ``@block';
		replace: '``@collection select: [:`each | | `@temps | ``@.Statements. ``@object not]'
			with: '``@collection reject: [:`each | | `@temps | ``@.Statements. ``@object]';
		replace: '``@collection reject: [:`each | | `@temps | ``@.Statements. ``@object not]'
			with: '``@collection select: [:`each | | `@temps | ``@.Statements. ``@object]';
		replace: '[| `@temps | ``@.Statements. ``@object not] whileTrue: ``@block'
			with: '[| `@temps | ``@.Statements. ``@object] whileFalse: ``@block';
		replace: '[| `@temps | ``@.Statements. ``@object not] whileFalse: ``@block'
			with: '[| `@temps | ``@.Statements. ``@object] whileTrue: ``@block';
		replace: '[| `@temps | ``@.Statements. ``@object not] whileTrue'
			with: '[| `@temps | ``@.Statements. ``@object] whileFalse';
		replace: '[| `@temps | ``@.Statements. ``@object not] whileFalse'
			with: '[| `@temps | ``@.Statements. ``@object] whileTrue';
		replace: '(``@a <= ``@b) not' with: '``@a > ``@b';
		replace: '(``@a < ``@b) not' with: '``@a >= ``@b';
		replace: '(``@a = ``@b) not' with: '``@a ~= ``@b';
		replace: '(``@a == ``@b) not' with: '``@a ~~ ``@b';
		replace: '(``@a ~= ``@b) not' with: '``@a = ``@b';
		replace: '(``@a ~~ ``@b) not' with: '``@a == ``@b';
		replace: '(``@a >= ``@b) not' with: '``@a < ``@b';
		replace: '(``@a > ``@b) not' with: '``@a <= ``@b'