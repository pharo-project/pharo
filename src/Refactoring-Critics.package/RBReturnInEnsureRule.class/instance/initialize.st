initialization
initialize
	| returnMatcher |
	super initialize.
	returnMatcher := RBParseTreeSearcher new.
	returnMatcher 
		matches: '^ `@object'
		do: [ :node :answer | true ].
	self matcher 
		matchesAnyOf: #(
			'``@rcv ensure: [| `@temps | ``@.Stmts]'
			'``@rcv ifCurtailed: [| `@temps | ``@.Stmts]')
		do: [ :node :answer | 
			answer isNil 
				ifTrue: [
					(returnMatcher executeTree: node arguments first initialAnswer: false) 
						ifTrue: [ node ]
						ifFalse: [ nil ] ]
				ifFalse: [ answer ] ]