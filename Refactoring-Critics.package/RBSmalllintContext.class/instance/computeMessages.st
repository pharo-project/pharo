private
computeMessages
	| searcher |
	selfMessages := Set new.
	superMessages := Set new.
	messages := Set new.
	searcher := RBParseTreeSearcher new.
	searcher
		matches: 'self `@message: ``@args'
			do: [:aNode :answer | selfMessages add: aNode selector];
		matches: 'super `@message: ``@args'
			do: [:aNode :answer | superMessages add: aNode selector];
		matches: '``@receiver `@message: ``@args'
			do: [:aNode :answer | messages add: aNode selector].
	searcher executeTree: self parseTree initialAnswer: nil