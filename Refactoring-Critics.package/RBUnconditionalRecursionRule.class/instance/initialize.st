initialization
initialize
	super initialize.
	self matcher
		matchesMethod: '`@message: `@args
			| `@temps |
			`@.before.
			self `@message: `@args.
			`@.after'
		do: [ :node :answer | 
			| index |
			index := node body statements
				findFirst: [ :each | each isMessage and: [ each selector = node selector ] ].
			((node body statements copyFrom: 1 to: index)
				anySatisfy: [ :each | each containsReturn ])
					ifTrue: [ answer ]
					ifFalse: [ node ] ]