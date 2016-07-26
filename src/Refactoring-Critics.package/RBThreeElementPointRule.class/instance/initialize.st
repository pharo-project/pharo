initialization
initialize
	super initialize.
	self matcher
		matches: '``@x @ ``@y'
		do: [ :node :answer | 
			answer isNil 
				ifTrue: [
					| current |
					current := node parent.
					[ current isNil or: [ current isMessage and: [ current selector = #@ or: [ current selector isInfix not ] ] ] ] 
						whileFalse: [ current := current parent ].
					(current isNil or: [ current isMessage and: [ current selector isInfix not ] ]) 
						ifTrue: [ nil ]
						ifFalse: [ node ] ]
				ifFalse: [ answer ] ]