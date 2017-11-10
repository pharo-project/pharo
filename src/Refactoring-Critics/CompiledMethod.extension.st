Extension { #name : #CompiledMethod }

{ #category : #'*Refactoring-Critics' }
CompiledMethod >> selfMessages [

	|  selfMessages searcher |
	selfMessages := Set new.
	searcher := RBParseTreeSearcher new.
	searcher
		matches: 'self `@message: ``@args'
			do: [:aNode :answer | selfMessages add: aNode selector].

	searcher executeTree: self parseTree initialAnswer: nil.
	
	^ selfMessages
]

{ #category : #'*Refactoring-Critics' }
CompiledMethod >> superMessages [

	|  superMessages searcher |
	superMessages := Set new.
	searcher := RBParseTreeSearcher new.
	searcher
		matches: 'super `@message: ``@args'
			do: [:aNode :answer | superMessages add: aNode selector].

	searcher executeTree: self parseTree initialAnswer: nil.
	
	^ superMessages
]
