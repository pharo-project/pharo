initialization
initialize
	super initialize.
	self matcher 
		matchesAnyOf: #(
			'`@object do: [:`each | | `@temps | ``@.Statements]' 
			'`@object collect: [:`each | | `@temps | ``@.Statements]' 
			'`@object select: [:`each | | `@temps | ``@.Statements]' 
			'`@object reject: [:`each | | `@temps | ``@.Statements]' 
			'`@object inject: `@value into: [:`sum :`each | | `@temps | ``@.Statements]')
		do: [ :node :answer | 
			answer isNil 
				ifTrue: [
					(self modifiesTree: node receiver in: node arguments last) 
						ifTrue: [ node ]
						ifFalse: [ nil ] ]
				ifFalse: [ answer ] ]