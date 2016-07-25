initialization
initialize
	super initialize.
	self matcher 
		matches: '1 to: ``@object size do: [:`each | | `@temps | `@.Statements]'
		do: [ :node :answer | 
			answer isNil 
				ifTrue: [
					| varName variableMatcher |
					varName := node arguments last arguments first.	"`each"
					variableMatcher := RBParseTreeSearcher new.
					variableMatcher 
						matchesTree: varName
						do: [ :nod :ans | ans and: [ nod parent isMessage and: [ nod parent selector = #at: ] ] ].
					(variableMatcher executeTree: node arguments last body initialAnswer: true) 
						ifTrue: [ node ]
						ifFalse: [ nil ] ]
				ifFalse: [ answer ] ]