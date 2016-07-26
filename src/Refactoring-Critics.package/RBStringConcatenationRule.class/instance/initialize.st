initialization
initialize
	| concatenationMatcher |
	super initialize.
	concatenationMatcher := RBParseTreeSearcher new.
	concatenationMatcher 
		matches: '`@receiver , `@argument'
		do: [ :node :answer | true ].
	self matcher 
		matchesAnyOf: #(
			'``@collection do: ``@argument' 
			'``@collection do: ``@argument1 separatedBy: ``@argument2' 
			'``@start to: ``@stop do: ``@argument' 
			'``@collection detect: ``@argument' 
			'``@collection detect: ``@argument1 ifNone: ``@argument2' 
			'``@collection select: ``@argument'
			'``@collection reject: ``@argument' 
			'``@collection inject: ``@value into: ``@argument'
			'``@collection anySatisfy: ``@argument'
			'``@collection allSatisfy: ``@argument'
			'``@collection noneSatisfy: ``@argument' )
		do: [ :node :answer | 
			answer isNil 
				ifTrue: [
					(node arguments 
						detect: [ :each | each isBlock and: [ concatenationMatcher executeTree: each initialAnswer: false ] ]
						ifNone: [ nil ]) notNil 
							ifTrue: [ node ]
							ifFalse: [ nil ] ]
				ifFalse: [ answer ] ]