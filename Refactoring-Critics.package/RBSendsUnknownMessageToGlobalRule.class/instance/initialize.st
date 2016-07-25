initialization
initialize
	super initialize.
	self matcher 
		matches: '`{:node :context | node isVariable and: [ Smalltalk includesKey: node name asSymbol ] } `@message: `@args'
		do: [ :node :answer | 
			answer isNil 
				ifTrue: [
					| what |
					what := Smalltalk globals at: node receiver name asSymbol.
					(what notNil and: [ (what respondsTo: node selector) not ]) 
						ifTrue: [ node ]
						ifFalse: [ nil ] ]
				ifFalse: [ answer ] ]