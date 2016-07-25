equivalence classes
computeEquivalenceClassesForMethodsAndVars
	| searcher |
	backpointers := self backpointersDictionary.
	class instanceVariableNames 
		do: [:each | backpointers at: each put: (self backpointersSetWith: each)].
	class withAllSubclasses do: 
		[:sub | sub selectors do: [:each | backpointers at: each put: (self backpointersSetWith: each)]].
	searcher := RBParseTreeSearcher new.
	searcher matches: '^``@object'
		do: [:aNode :answer | self processNode: aNode value].
	self executeSearch: searcher